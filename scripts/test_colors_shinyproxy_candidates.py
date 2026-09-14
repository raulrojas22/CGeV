#!/usr/bin/env python3

from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess
import sys
import tempfile

from verify_colors_image_startup import candidate_command


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "scripts" / "build_colors_shinyproxy_candidates.py"
SPEC = importlib.util.spec_from_file_location("colors_candidates", HELPER)
assert SPEC and SPEC.loader
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


APPLICATION = """proxy:
  title: Preserved title
  specs:
    - id: other
      container-env:
        KEEP_OTHER: "yes"
    - id: cgv
      display-name: Preserved display
      container-env:
        KEEP_APPLICATION: "unchanged"
      container-volumes:
        - "/data:/app/data:ro"
logging:
  file: /tmp/proxy.log
"""

COMPOSE = """services:
  socket-proxy:
    image: preserved/socket
  shinyproxy:
    image: preserved/proxy
    volumes:
      - ./application.yml:/opt/shinyproxy/application.yml:ro
    environment:
      KEEP_COMPOSE: "unchanged"
    networks:
      - sp-net
  worker:
    environment:
      APP_ASSET_VERSION: "worker-value-is-unrelated"
networks:
  sp-net: {}
"""


application_candidate = MODULE.build_application_candidate(APPLICATION)
expected_application = APPLICATION.replace(
    "      display-name: Preserved display\n      container-env:\n",
    "      display-name: Preserved display\n"
    "      container-env:\n"
    '        APP_ASSET_VERSION: "${APP_ASSET_VERSION:}"\n'
    '        APP_STATIC_BASE_URL: "${APP_STATIC_BASE_URL:}"\n'
    '        APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"\n',
)
expected_application = expected_application.replace(
    "    - id: cgv\n", '    - id: cgv\n      container-cmd: ["bash", "/app/deploy/docker/run-app.sh"]\n'
)
assert application_candidate == expected_application
assert '        APP_ASSET_VERSION: "${APP_ASSET_VERSION:}"\n' in application_candidate
assert '        APP_STATIC_BASE_URL: "${APP_STATIC_BASE_URL:}"\n' in application_candidate
assert '        APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"\n' in application_candidate
assert '        KEEP_APPLICATION: "unchanged"\n' in application_candidate
assert '        KEEP_OTHER: "yes"\n' in application_candidate
assert MODULE.build_application_candidate(application_candidate) == application_candidate

compose_candidate = MODULE.build_compose_candidate(COMPOSE)
expected_compose = COMPOSE.replace(
    "    environment:\n      KEEP_COMPOSE",
    "    environment:\n"
    '      APP_ASSET_VERSION: "${APP_ASSET_VERSION:-}"\n'
    '      APP_STATIC_BASE_URL: "${APP_STATIC_BASE_URL:-}"\n'
    '      APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"\n'
    "      KEEP_COMPOSE",
    1,
)
assert compose_candidate == expected_compose
assert '      APP_ASSET_VERSION: "${APP_ASSET_VERSION:-}"\n' in compose_candidate
assert '      APP_STATIC_BASE_URL: "${APP_STATIC_BASE_URL:-}"\n' in compose_candidate
assert '      APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"\n' in compose_candidate
assert '      KEEP_COMPOSE: "unchanged"\n' in compose_candidate
assert '      APP_ASSET_VERSION: "worker-value-is-unrelated"\n' in compose_candidate
assert MODULE.build_compose_candidate(compose_candidate) == compose_candidate

existing_application = application_candidate.replace(
    'APP_ASSET_VERSION: "${APP_ASSET_VERSION:}"', 'APP_ASSET_VERSION: "old"'
).replace(
    'APP_STATIC_BASE_URL: "${APP_STATIC_BASE_URL:}"', "APP_STATIC_BASE_URL: old"
).replace(
    'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"',
    'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY:0}"',
)
assert MODULE.build_application_candidate(existing_application) == application_candidate

existing_compose = compose_candidate.replace(
    'APP_ASSET_VERSION: "${APP_ASSET_VERSION:-}"', 'APP_ASSET_VERSION: "old"', 1
).replace(
    'APP_STATIC_BASE_URL: "${APP_STATIC_BASE_URL:-}"', "APP_STATIC_BASE_URL: old", 1
).replace(
    'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"',
    'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY:-0}"',
    1,
)
assert MODULE.build_compose_candidate(existing_compose) == compose_candidate

crlf_application = APPLICATION.replace("\n", "\r\n")
crlf_candidate = MODULE.build_application_candidate(crlf_application)
assert "\n" not in crlf_candidate.replace("\r\n", "")
assert 'APP_ASSET_VERSION: "${APP_ASSET_VERSION:}"\r\n' in crlf_candidate
assert 'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "0"\r\n' in crlf_candidate

# Regression: a missing server .env flag is resolved to 0 by the deploy before
# candidate generation. Neither ShinyProxy configuration layer may retain a
# same-name placeholder for Spring or podman-compose to resolve later.
assert "${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY" not in application_candidate
assert "${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY" not in compose_candidate
strict_application = MODULE.build_application_candidate(APPLICATION, "1")
strict_compose = MODULE.build_compose_candidate(COMPOSE, "1")
assert 'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "1"' in strict_application
assert 'APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "1"' in strict_compose
assert MODULE.build_application_candidate(strict_application, "1") == strict_application
assert MODULE.build_compose_candidate(strict_compose, "1") == strict_compose
try:
    MODULE.build_application_candidate(APPLICATION, "2")
except MODULE.CandidateError as error:
    assert "literal 0 or 1" in str(error)
else:
    raise AssertionError("an invalid orthology policy was accepted")


def rejected(builder, text: str, expected: str) -> None:
    try:
        builder(text)
    except MODULE.CandidateError as error:
        assert expected in str(error), (expected, str(error))
    else:
        raise AssertionError(f"unsafe input was accepted; expected {expected!r}")


command_line = '      container-cmd: ["bash", "/app/deploy/docker/run-app.sh"]\n'
legacy_application = application_candidate.replace("/app/deploy/docker/run-app.sh", "/app/docker/run-app.sh")
assert MODULE.build_application_candidate(legacy_application) == application_candidate
assert candidate_command(application_candidate) == ["bash", "/app/deploy/docker/run-app.sh"]
assert candidate_command(application_candidate.replace("- id: cgv", "- id: cgv # public")) == candidate_command(application_candidate)
for invalid_candidate in (APPLICATION, legacy_application):
    try:
        candidate_command(invalid_candidate)
    except ValueError:
        pass
    else:
        raise AssertionError("startup gate accepted a missing or obsolete command")
other_command = '      container-cmd: ["custom-other-app"]\n'
other_application = legacy_application.replace("    - id: other\n", "    - id: other\n" + other_command)
assert other_command in MODULE.build_application_candidate(other_application)
rejected(MODULE.build_application_candidate,
         application_candidate.replace(command_line, command_line * 2), "duplicate container-cmd")
rejected(MODULE.build_application_candidate,
         application_candidate.replace("/app/deploy/docker/run-app.sh", "/custom/start.sh"), "unknown container-cmd")
rejected(MODULE.build_application_candidate,
         application_candidate.replace(command_line, '      container-cmd:\n        - bash\n'), "known JSON command")
rejected(MODULE.build_application_candidate,
         application_candidate.replace(command_line, "  " + command_line), "direct cgv property")
rejected(MODULE.build_application_candidate,
         application_candidate.replace(command_line, command_line + "        nested: value\n"), "nested content")


rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace(
        "      container-env:\n",
        "      container-env-file: /opt/shinyproxy/env/cgv.env\n      container-env:\n",
        1,
    ),
    "container-env-file",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace("logging:\n", "    - id: cgv\n      container-env:\n        X: y\nlogging:\n"),
    "exactly one '- id: cgv'",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace("proxy:\n", "proxy:\n", 1) + "- id: cgv\n  container-env:\n    X: y\n",
    "exactly one '- id: cgv'",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace("proxy:\n", "proxy:\n", 1) + "proxy:\n  specs: []\n",
    "top-level proxy",
)
rejected(
    MODULE.build_application_candidate,
    application_candidate.replace(
        '        KEEP_APPLICATION: "unchanged"\n',
        '        APP_ASSET_VERSION: "duplicate"\n        KEEP_APPLICATION: "unchanged"\n',
    ),
    "duplicate APP_ASSET_VERSION",
)
rejected(
    MODULE.build_application_candidate,
    application_candidate.replace(
        '        KEEP_APPLICATION: "unchanged"\n',
        '        APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "1"\n        KEEP_APPLICATION: "unchanged"\n',
    ),
    "duplicate APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace(
        '      display-name: Preserved display\n',
        '      display-name: Preserved display\n      APP_ASSET_VERSION: "wrong-scope"\n',
    ),
    "outside cgv container-env",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace('        KEEP_APPLICATION: "unchanged"\n', "        APP_ASSET_VERSION:\n"),
    "one-line scalar",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace(
        "      display-name: Preserved display\n      container-env:\n",
        "      display-name: Preserved display\n      container-env: *shared\n",
    ),
    "flow, alias, or scalar",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace(
        "      display-name: Preserved display\n",
        "      display-name: Preserved display\n      <<: *unsafe-defaults\n",
    ),
    "merge keys in the cgv spec",
)
rejected(
    MODULE.build_application_candidate,
    APPLICATION.replace(
        '        KEEP_APPLICATION: "unchanged"\n',
        "        - APP_PREWARM_ON_START=0\n",
    ),
    "list form",
)

rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "      - ./application.yml:/opt/shinyproxy/application.yml:ro\n",
        "      - ./.env:/opt/shinyproxy/env/cgv.env:ro\n",
    ),
    "cgv.env mount",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "    image: preserved/proxy\n",
        "    image: preserved/proxy\n    env_file:\n      - .env\n",
    ),
    "shinyproxy env_file",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "    image: preserved/proxy\n",
        "    image: preserved/proxy\n    <<: *unsafe-defaults\n",
    ),
    "merge keys in shinyproxy",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "    image: preserved/proxy\n",
        "    image: preserved/proxy\n    extends: ./proxy-common.yml\n",
    ),
    "shinyproxy extends",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "      - ./application.yml:/opt/shinyproxy/application.yml:ro\n",
        "      - /home/rarojas/cgv/.env:/run/release-values:ro\n",
    ),
    ".env mount",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "      - ./application.yml:/opt/shinyproxy/application.yml:ro\n",
        "      - type: bind\n        source: .env\n        target: /run/release-values\n",
    ),
    ".env mount",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        "      - ./application.yml:/opt/shinyproxy/application.yml:ro\n",
        "      - type: bind\n        source: ./.env\n        target: /opt/shinyproxy/env/cgv.env\n",
    ),
    ".env mount",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace("  worker:\n", "  shinyproxy:\n    environment:\n      X: y\n  worker:\n"),
    "exactly one shinyproxy",
)
rejected(
    MODULE.build_compose_candidate,
    compose_candidate.replace(
        '      KEEP_COMPOSE: "unchanged"\n',
        '      APP_STATIC_BASE_URL: "duplicate"\n      KEEP_COMPOSE: "unchanged"\n',
    ),
    "duplicate APP_STATIC_BASE_URL",
)
rejected(
    MODULE.build_compose_candidate,
    compose_candidate.replace(
        '      KEEP_COMPOSE: "unchanged"\n',
        '      APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: "1"\n      KEEP_COMPOSE: "unchanged"\n',
    ),
    "duplicate APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace("    environment:\n", "    environment: *shared\n", 1),
    "flow, alias, or scalar",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace(
        '      KEEP_COMPOSE: "unchanged"\n',
        "      - KEEP_COMPOSE=unchanged\n",
    ),
    "list form",
)
rejected(
    MODULE.build_compose_candidate,
    COMPOSE.replace("      KEEP_COMPOSE", "\t     KEEP_COMPOSE"),
    "tab indentation",
)

with tempfile.TemporaryDirectory(prefix="colors-candidates-") as temp_dir:
    temp = Path(temp_dir)
    application = temp / "application.yml"
    compose = temp / "compose.yml"
    application_output = temp / "application.yml.candidate"
    compose_output = temp / "compose.yml.candidate"
    application.write_text(APPLICATION, encoding="utf-8")
    compose.write_text(COMPOSE, encoding="utf-8")
    application.chmod(0o640)
    compose.chmod(0o600)
    subprocess.run(
        [
            sys.executable,
            "-B",
            str(HELPER),
            "--application",
            str(application),
            "--application-output",
            str(application_output),
            "--compose",
            str(compose),
            "--compose-output",
            str(compose_output),
            "--orthology-policy",
            "0",
        ],
        check=True,
    )
    assert application_output.read_text(encoding="utf-8") == application_candidate
    assert compose_output.read_text(encoding="utf-8") == compose_candidate
    assert application_output.stat().st_mode & 0o777 == 0o640
    assert compose_output.stat().st_mode & 0o777 == 0o600
    assert application.read_text(encoding="utf-8") == APPLICATION
    assert compose.read_text(encoding="utf-8") == COMPOSE

    application_output.unlink()
    compose_output.unlink()
    unsafe_compose = COMPOSE.replace(
        "      - ./application.yml:/opt/shinyproxy/application.yml:ro\n",
        "      - ./.env:/opt/shinyproxy/env/cgv.env:ro\n",
    )
    compose.write_text(unsafe_compose, encoding="utf-8")
    failed = subprocess.run(
        [
            sys.executable,
            "-B",
            str(HELPER),
            "--application",
            str(application),
            "--application-output",
            str(application_output),
            "--compose",
            str(compose),
            "--compose-output",
            str(compose_output),
            "--orthology-policy",
            "0",
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    assert failed.returncode != 0
    assert not application_output.exists()
    assert not compose_output.exists()
    assert "cgv.env mount" in failed.stderr

# A real cap must migrate only the selected app, be idempotent, and reject
# ambiguous YAML instead of silently leaving a memory setting ineffective.
legacy_memory = APPLICATION.replace("      display-name: Preserved display",
    "      display-name: Preserved display\n      container-memory: ${SP_CONTAINER_MEMORY:2g}")
limited = MODULE.build_application_candidate(legacy_memory, memory_limit="2g")
assert '      container-memory-limit: "2g"' in limited
assert '      container-memory:' not in limited
assert MODULE.build_application_candidate(limited, memory_limit="2g") == limited
assert 'container-memory-limit: "3g"' in MODULE.build_application_candidate(limited, memory_limit="3g")
assert 'container-memory-limit: "2048m"' in MODULE.build_application_candidate(APPLICATION, memory_limit="2048m")
other_app = legacy_memory
# The fixture has a second app; a cap there must survive verbatim.
other_app = other_app.replace('        KEEP_OTHER: "yes"',
    '        KEEP_OTHER: "yes"\n      container-memory: 7g')
assert '      container-memory: 7g' in MODULE.build_application_candidate(other_app, memory_limit="2g")
for invalid_text, limit in [
    (legacy_memory, "0g"), (legacy_memory, "2g;false"), (legacy_memory, "2GB"),
    (legacy_memory.replace('      container-env:', '      container-memory-limit: 4g\n      container-env:'), "2g"),
    (legacy_memory.replace('      container-memory:', '        container-memory:'), "2g"),
    (legacy_memory.replace('${SP_CONTAINER_MEMORY:2g}', '|\n        2g'), "2g"),
]:
    try:
        MODULE.build_application_candidate(invalid_text, memory_limit=limit)
    except MODULE.CandidateError:
        pass
    else:
        raise AssertionError("Ambiguous/invalid memory limit was accepted")

print("Colors ShinyProxy candidates: OK (including effective memory migration)")
