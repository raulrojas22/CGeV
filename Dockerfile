ARG CGV_DEPS_IMAGE=cgv-deps:1.0.0
FROM ${CGV_DEPS_IMAGE}

LABEL org.opencontainers.image.title="CGeV" \
      org.opencontainers.image.description="Comparative Gene Viewer — interactive Shiny app for gene structure, alignment, and comparative genomics" \
      org.opencontainers.image.version="1.1.0" \
      org.opencontainers.image.authors="Raul Rojas-Espinoza" \
      org.opencontainers.image.source="https://github.com/raulrojas22/CGeV" \
      org.opencontainers.image.licenses="MIT"

WORKDIR /app

COPY . /app
# Compile immutable application code once per image, before any user session.
RUN Rscript scripts/compile_runtime.R /app/.cgv-compiled
# ShinyProxy launches application containers with an unprivileged runtime UID.
# Normalize read/traverse permissions after COPY so static assets keep working
# even if a local file arrived with owner-only permissions (for example 0600).
RUN chmod -R a+rX /app \
    && chmod +x /app/deploy/docker/run-app.sh

ENV APP_DIR=/app \
    APP_HOST=0.0.0.0 \
    APP_PORT=3838 \
    APP_DEBUG_LOGS=0 \
    APP_PERF_TIMING=0

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=5 \
  CMD wget -qO- "http://127.0.0.1:${APP_PORT}/healthz.txt" 2>/dev/null | grep -qx 'ok' || exit 1

CMD ["/app/deploy/docker/run-app.sh"]
