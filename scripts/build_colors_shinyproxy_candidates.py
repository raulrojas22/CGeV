#!/usr/bin/env python3
"""Build fail-closed candidates for the server-owned Colors YAML files.

The server files intentionally remain the source of truth.  This helper only
adds or normalizes a small non-secret allowlist required by the public app; it
never reads an env file and it refuses legacy env-file mounts that could
silently override those variables.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import stat
import tempfile
from typing import Iterable


APP_VALUES = (
    ("APP_ASSET_VERSION", '"${APP_ASSET_VERSION:}"'),
    ("APP_STATIC_BASE_URL", '"${APP_STATIC_BASE_URL:}"'),
)
COMPOSE_VALUES = (
    ("APP_ASSET_VERSION", '"${APP_ASSET_VERSION:-}"'),
    ("APP_STATIC_BASE_URL", '"${APP_STATIC_BASE_URL:-}"'),
)
ORTHOLOGY_POLICY_KEY = "APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY"
ENV_FILE_MOUNT_RE = re.compile(r"(?<![A-Za-z0-9_.])\.env(?![A-Za-z0-9_.-])")


class CandidateError(ValueError):
    """The input cannot be transformed without making an unsafe assumption."""


def _resolved_values(
    values: tuple[tuple[str, str], ...], orthology_policy: str
) -> tuple[tuple[str, str], ...]:
    if orthology_policy not in {"0", "1"}:
        raise CandidateError("orthology policy must be the literal 0 or 1")
    return (*values, (ORTHOLOGY_POLICY_KEY, f'"{orthology_policy}"'))


def _body(line: str) -> str:
    if line.endswith("\r\n"):
        return line[:-2]
    if line.endswith(("\n", "\r")):
        return line[:-1]
    return line


def _ending(line: str) -> str:
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    if line.endswith("\r"):
        return "\r"
    return ""


def _prepare(text: str, label: str) -> list[str]:
    if text.startswith("\ufeff"):
        raise CandidateError(f"{label}: UTF-8 BOM is not accepted")
    lines = text.splitlines(keepends=True)
    if not lines and text == "":
        raise CandidateError(f"{label}: input is empty")
    for number, line in enumerate(lines, start=1):
        if _ending(line) == "\r":
            raise CandidateError(f"{label}: bare CR line ending at line {number}")
        raw = _body(line)
        prefix = raw[: len(raw) - len(raw.lstrip(" \t"))]
        if "\t" in prefix:
            raise CandidateError(f"{label}: tab indentation at line {number}")
    return lines


def _active(line: str) -> bool:
    stripped = _body(line).lstrip(" ")
    return bool(stripped) and not stripped.startswith("#")


def _indent(line: str) -> int:
    raw = _body(line)
    return len(raw) - len(raw.lstrip(" "))


def _key_pattern(key: str, *, value_required: bool = False) -> re.Pattern[str]:
    escaped = re.escape(key)
    value = r"(?P<value>.+?)" if value_required else r"(?P<value>.*?)"
    return re.compile(
        rf"^(?P<indent> *)(?:{escaped}|'{escaped}'|\"{escaped}\")\s*:\s*{value}\s*$"
    )


def _key_matches(lines: Iterable[str], key: str) -> list[tuple[int, re.Match[str]]]:
    pattern = _key_pattern(key)
    matches: list[tuple[int, re.Match[str]]] = []
    for index, line in enumerate(lines):
        if not _active(line):
            continue
        match = pattern.match(_body(line))
        if match:
            matches.append((index, match))
    return matches


def _mapping_anchor_matches(
    lines: list[str],
    key: str,
    *,
    start: int = 0,
    end: int | None = None,
    indent: int | None = None,
) -> list[int]:
    matches: list[int] = []
    pattern = _key_pattern(key)
    stop = len(lines) if end is None else end
    for index in range(start, stop):
        line = lines[index]
        if not _active(line):
            continue
        if indent is not None and _indent(line) != indent:
            continue
        match = pattern.match(_body(line))
        if not match:
            continue
        remainder = match.group("value").strip()
        if remainder and not remainder.startswith("#"):
            raise CandidateError(f"{key}: flow, alias, or scalar form is not accepted")
        matches.append(index)
    return matches


def _block_end(lines: list[str], anchor: int, anchor_indent: int) -> int:
    for index in range(anchor + 1, len(lines)):
        if _active(lines[index]) and _indent(lines[index]) <= anchor_indent:
            return index
    return len(lines)


def _direct_indent(lines: list[str], start: int, end: int, label: str) -> int:
    indents = [_indent(lines[index]) for index in range(start, end) if _active(lines[index])]
    if not indents:
        return _indent(lines[start - 1]) + 2
    direct = min(indents)
    if direct <= _indent(lines[start - 1]):
        raise CandidateError(f"{label}: malformed mapping indentation")
    return direct


def _validate_scalar_target(line: str, key: str, label: str) -> None:
    match = _key_pattern(key, value_required=True).match(_body(line))
    if not match:
        raise CandidateError(f"{label}: {key} must be a one-line scalar")
    value = match.group("value").strip()
    if not value or value.startswith("#") or value[0] in "|>&*!{[":
        raise CandidateError(f"{label}: {key} must be a plain one-line scalar")


def _has_direct_key(
    lines: list[str], start: int, end: int, direct_indent: int, key: str
) -> bool:
    return any(
        _indent(lines[index + start]) == direct_indent
        for index, _match in _key_matches(lines[start:end], key)
    )


def _upsert_mapping(
    lines: list[str],
    *,
    anchor: int,
    end: int,
    values: tuple[tuple[str, str], ...],
    label: str,
) -> list[str]:
    direct_indent = _direct_indent(lines, anchor + 1, end, label)
    replacements: dict[int, str] = {}
    missing: list[tuple[str, str]] = []

    if any(
        _active(lines[index])
        and _indent(lines[index]) == direct_indent
        and _body(lines[index]).lstrip(" ").startswith("-")
        for index in range(anchor + 1, end)
    ):
        raise CandidateError(f"{label}: list form is not accepted")

    if any(
        _active(lines[index])
        and _indent(lines[index]) == direct_indent
        and _key_pattern("<<").match(_body(lines[index]))
        for index in range(anchor + 1, end)
    ):
        raise CandidateError(f"{label}: YAML merge keys are not accepted")

    for key, value in values:
        matches = [
            (index + anchor + 1, match)
            for index, match in _key_matches(lines[anchor + 1 : end], key)
        ]
        if len(matches) > 1:
            raise CandidateError(f"{label}: duplicate {key}")
        if not matches:
            missing.append((key, value))
            continue
        index, match = matches[0]
        if int(len(match.group("indent"))) != direct_indent:
            raise CandidateError(f"{label}: {key} is not a direct mapping entry")
        _validate_scalar_target(lines[index], key, label)
        for child in range(index + 1, end):
            if not _active(lines[child]):
                continue
            if _indent(lines[child]) > direct_indent:
                raise CandidateError(f"{label}: {key} must not have nested content")
            break
        replacements[index] = " " * direct_indent + f"{key}: {value}" + _ending(lines[index])

    if missing and not _ending(lines[anchor]):
        raise CandidateError(f"{label}: mapping anchor has no line ending")

    output: list[str] = []
    for index, line in enumerate(lines):
        output.append(replacements.get(index, line))
        if index == anchor:
            newline = _ending(line)
            output.extend(
                " " * direct_indent + f"{key}: {value}" + newline
                for key, value in missing
            )
    return output


def build_application_candidate(
    text: str, orthology_policy: str = "0", memory_limit: str | None = None
) -> str:
    lines = _prepare(text, "application.yml")
    values = _resolved_values(APP_VALUES, orthology_policy)
    if _key_matches(lines, "container-env-file"):
        raise CandidateError("application.yml: container-env-file is not accepted")

    proxies = _mapping_anchor_matches(lines, "proxy", indent=0)
    if len(proxies) != 1:
        raise CandidateError("application.yml: expected exactly one top-level proxy mapping")
    proxy = proxies[0]
    proxy_end = _block_end(lines, proxy, 0)
    proxy_child_indent = _direct_indent(lines, proxy + 1, proxy_end, "application.yml proxy")
    specs = _mapping_anchor_matches(
        lines,
        "specs",
        start=proxy + 1,
        end=proxy_end,
        indent=proxy_child_indent,
    )
    if len(specs) != 1:
        raise CandidateError("application.yml: expected exactly one proxy specs mapping")
    specs_anchor = specs[0]
    specs_end = _block_end(lines, specs_anchor, _indent(lines[specs_anchor]))
    spec_indent = _direct_indent(lines, specs_anchor + 1, specs_end, "application.yml specs")

    cgv_pattern = re.compile(r"^(?P<indent> *)-\s+id\s*:\s*(?:cgv|'cgv'|\"cgv\")\s*(?:#.*)?$")
    cgv_specs = [
        (index, match)
        for index, line in enumerate(lines)
        if _active(line) and (match := cgv_pattern.match(_body(line)))
    ]
    if (
        len(cgv_specs) != 1
        or not specs_anchor < cgv_specs[0][0] < specs_end
        or len(cgv_specs[0][1].group("indent")) != spec_indent
    ):
        raise CandidateError("application.yml: expected exactly one '- id: cgv' spec")
    spec, spec_match = cgv_specs[0]
    assert len(spec_match.group("indent")) == spec_indent
    spec_end = _block_end(lines, spec, spec_indent)
    spec_child_indent = _direct_indent(lines, spec + 1, spec_end, "application.yml cgv spec")
    if _has_direct_key(lines, spec + 1, spec_end, spec_child_indent, "<<"):
        raise CandidateError("application.yml: YAML merge keys in the cgv spec are not accepted")

    if memory_limit is not None:
        if not re.fullmatch(r"[1-9][0-9]*[mMgG]", memory_limit):
            raise CandidateError("application.yml: memory limit must be positive MiB/GiB (e.g. 2g)")
        memory_keys = ("container-memory", "container-memory-limit")
        matches = [(index + spec + 1, key) for key in memory_keys
                   for index, _ in _key_matches(lines[spec + 1:spec_end], key)]
        if len(matches) > 1:
            raise CandidateError("application.yml: duplicate or competing container memory keys")
        if matches:
            index, key = matches[0]
            if _indent(lines[index]) != spec_child_indent:
                raise CandidateError("application.yml: memory limit is outside cgv spec properties")
            _validate_scalar_target(lines[index], key, "application.yml cgv spec")
            # Rename only the legacy property in this spec. The generic upsert
            # below also rejects nested YAML and preserves all unrelated text.
            match = _key_pattern(key).match(_body(lines[index]))
            lines[index] = (" " * spec_child_indent + "container-memory-limit: "
                            + match.group("value") + _ending(lines[index]))
        lines = _upsert_mapping(lines, anchor=spec, end=spec_end,
                                values=(("container-memory-limit", f'"{memory_limit}"'),),
                                label="application.yml cgv spec")
        spec_end = _block_end(lines, spec, spec_indent)

    # ShinyProxy overrides the image CMD. Migrate only the known entrypoint
    # from before the repository reorganization; reject custom commands.
    command = ["bash", "/app/deploy/docker/run-app.sh"]
    commands = [(index + spec + 1, match) for index, match in
                _key_matches(lines[spec + 1:spec_end], "container-cmd")]
    if len(commands) > 1:
        raise CandidateError("application.yml: duplicate container-cmd")
    if commands:
        index, match = commands[0]
        if _indent(lines[index]) != spec_child_indent:
            raise CandidateError("application.yml: container-cmd is not a direct cgv property")
        try:
            current_command = json.loads(match.group("value"))
        except (ValueError, TypeError) as error:
            raise CandidateError("application.yml: container-cmd must be a known JSON command") from error
        if current_command not in (command, ["bash", "/app/docker/run-app.sh"]):
            raise CandidateError("application.yml: unknown container-cmd; refusing to replace it")
        for child in range(index + 1, spec_end):
            if not _active(lines[child]):
                continue
            if _indent(lines[child]) > spec_child_indent:
                raise CandidateError("application.yml: container-cmd must not have nested content")
            break
        lines[index] = (" " * spec_child_indent + "container-cmd: "
                        + json.dumps(command) + _ending(lines[index]))
    else:
        lines.insert(spec + 1, " " * spec_child_indent + "container-cmd: "
                     + json.dumps(command) + _ending(lines[spec]))
    spec_end = _block_end(lines, spec, spec_indent)

    anchors = [
        index
        for index in _mapping_anchor_matches(
            lines, "container-env", start=spec + 1, end=spec_end
        )
    ]
    if len(anchors) != 1:
        raise CandidateError("application.yml: expected one cgv container-env mapping")
    anchor = anchors[0]
    if _indent(lines[anchor]) <= spec_indent:
        raise CandidateError("application.yml: container-env is outside the cgv spec")
    end = _block_end(lines, anchor, _indent(lines[anchor]))

    for key, _ in values:
        in_spec = [
            index + spec + 1
            for index, _match in _key_matches(lines[spec + 1 : spec_end], key)
        ]
        in_mapping = [index for index in in_spec if anchor < index < end]
        if len(in_spec) != len(in_mapping):
            raise CandidateError(f"application.yml: {key} is outside cgv container-env")

    return "".join(
        _upsert_mapping(
            lines,
            anchor=anchor,
            end=end,
            values=values,
            label="application.yml cgv container-env",
        )
    )


def build_compose_candidate(text: str, orthology_policy: str = "0") -> str:
    lines = _prepare(text, "compose.yml")
    values = _resolved_values(COMPOSE_VALUES, orthology_policy)
    services = _mapping_anchor_matches(lines, "services", indent=0)
    if len(services) != 1:
        raise CandidateError("compose.yml: expected exactly one top-level services mapping")
    services_anchor = services[0]
    services_end = _block_end(lines, services_anchor, 0)
    service_indent = _direct_indent(lines, services_anchor + 1, services_end, "services")

    shinyproxy = _mapping_anchor_matches(
        lines,
        "shinyproxy",
        start=services_anchor + 1,
        end=services_end,
        indent=service_indent,
    )
    if len(shinyproxy) != 1:
        raise CandidateError("compose.yml: expected exactly one shinyproxy service")
    service = shinyproxy[0]
    service_end = _block_end(lines, service, service_indent)
    child_indent = _direct_indent(lines, service + 1, service_end, "shinyproxy service")

    if _has_direct_key(lines, service + 1, service_end, child_indent, "<<"):
        raise CandidateError("compose.yml: YAML merge keys in shinyproxy are not accepted")
    if _has_direct_key(lines, service + 1, service_end, child_indent, "extends"):
        raise CandidateError("compose.yml: shinyproxy extends is not accepted")

    env_file_keys = [
        index + service + 1
        for index, _match in _key_matches(lines[service + 1 : service_end], "env_file")
        if _indent(lines[index + service + 1]) == child_indent
    ]
    if env_file_keys:
        raise CandidateError("compose.yml: shinyproxy env_file is not accepted")

    volume_keys = [
        index + service + 1
        for index, _match in _key_matches(lines[service + 1 : service_end], "volumes")
        if _indent(lines[index + service + 1]) == child_indent
    ]
    if len(volume_keys) > 1:
        raise CandidateError("compose.yml: duplicate shinyproxy volumes mapping")
    if volume_keys:
        volume = volume_keys[0]
        volume_match = _key_pattern("volumes").match(_body(lines[volume]))
        assert volume_match is not None
        remainder = volume_match.group("value").strip()
        if remainder and not remainder.startswith("#"):
            raise CandidateError("compose.yml: inline or aliased shinyproxy volumes are not accepted")
        volume_end = _block_end(lines, volume, _indent(lines[volume]))
        for index in range(volume + 1, volume_end):
            if not _active(lines[index]):
                continue
            volume_line = _body(lines[index])
            if "cgv.env" in volume_line.lower():
                raise CandidateError("compose.yml: a shinyproxy cgv.env mount is not accepted")
            if ENV_FILE_MOUNT_RE.search(volume_line):
                raise CandidateError("compose.yml: a shinyproxy .env mount is not accepted")

    environments = _mapping_anchor_matches(
        lines,
        "environment",
        start=service + 1,
        end=service_end,
        indent=child_indent,
    )
    if len(environments) != 1:
        raise CandidateError("compose.yml: expected one shinyproxy environment mapping")
    anchor = environments[0]
    end = _block_end(lines, anchor, _indent(lines[anchor]))

    for key, _ in values:
        in_service = [
            index + service + 1
            for index, _match in _key_matches(lines[service + 1 : service_end], key)
        ]
        in_mapping = [index for index in in_service if anchor < index < end]
        if len(in_service) != len(in_mapping):
            raise CandidateError(f"compose.yml: {key} is outside shinyproxy environment")

    return "".join(
        _upsert_mapping(
            lines,
            anchor=anchor,
            end=end,
            values=values,
            label="compose.yml shinyproxy environment",
        )
    )


def _read_input(path: Path, label: str) -> tuple[str, os.stat_result]:
    if path.is_symlink() or not path.is_file():
        raise CandidateError(f"{label}: input must be a regular non-symlink file")
    try:
        return path.read_bytes().decode("utf-8"), path.stat()
    except UnicodeDecodeError as error:
        raise CandidateError(f"{label}: input must be valid UTF-8") from error


def _stage_candidate(path: Path, content: str, source_stat: os.stat_result) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(content.encode("utf-8"))
            handle.flush()
            os.fsync(handle.fileno())
        try:
            os.chown(temporary, source_stat.st_uid, source_stat.st_gid)
        except PermissionError as error:
            raise CandidateError("cannot preserve server-owned uid/gid on candidate") from error
        os.chmod(temporary, stat.S_IMODE(source_stat.st_mode), follow_symlinks=False)
        os.utime(
            temporary,
            ns=(source_stat.st_atime_ns, source_stat.st_mtime_ns),
            follow_symlinks=False,
        )
        return temporary
    except BaseException:
        try:
            temporary.unlink()
        except FileNotFoundError:
            pass
        raise


def write_candidates(
    application: Path,
    application_output: Path,
    compose: Path,
    compose_output: Path,
    orthology_policy: str,
    memory_limit: str | None = None,
) -> None:
    resolved = [path.resolve() for path in (application, application_output, compose, compose_output)]
    if len(set(resolved)) != 4:
        raise CandidateError("inputs and outputs must be four different paths")
    if application_output.is_symlink() or compose_output.is_symlink():
        raise CandidateError("candidate outputs must not be symlinks")

    application_text, application_stat = _read_input(application, "application.yml")
    compose_text, compose_stat = _read_input(compose, "compose.yml")

    # Validate and build both documents before creating either output.
    application_candidate = build_application_candidate(application_text, orthology_policy, memory_limit)
    compose_candidate = build_compose_candidate(compose_text, orthology_policy)

    staged: list[tuple[Path, Path]] = []
    try:
        staged.append(
            (_stage_candidate(application_output, application_candidate, application_stat), application_output)
        )
        staged.append((_stage_candidate(compose_output, compose_candidate, compose_stat), compose_output))
        for temporary, output in staged:
            os.replace(temporary, output)
    finally:
        for temporary, _output in staged:
            try:
                temporary.unlink()
            except FileNotFoundError:
                pass


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Build safe application.yml and Compose candidates for Colors"
    )
    parser.add_argument("--application", required=True, type=Path)
    parser.add_argument("--application-output", required=True, type=Path)
    parser.add_argument("--compose", required=True, type=Path)
    parser.add_argument("--compose-output", required=True, type=Path)
    parser.add_argument("--orthology-policy", required=True, choices=("0", "1"))
    parser.add_argument("--container-memory-limit", help="Explicit measured app limit, e.g. 2g")
    args = parser.parse_args()
    write_candidates(
        args.application,
        args.application_output,
        args.compose,
        args.compose_output,
        args.orthology_policy,
        args.container_memory_limit,
    )


if __name__ == "__main__":
    main()
