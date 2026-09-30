/**
 * The compass payload both host test files build their model from.
 *
 * One copy, so the host transform and the browser's re-normalisation are
 * tested against the same shape. It is the object `compass env status --json`
 * prints, trimmed to the fields the plugin reads.
 */

export const COMPASS_PAYLOAD = {
  ok: true,
  generatedAt: '2026-09-30T12:45:00Z',
  env: {
    name: 'brave_hopper',
    label: 'brave_hopper',
    preset: 'linux-clang-debug-make',
    worktree: '/home/marco/Development/OreStudio/ores_dev_brave_hopper',
    envVersion: 26,
    requiredEnvVersion: 26,
    envStale: false,
    scope: 'dsh-1.scope',
    slice: 'app.slice',
    activities: [],
    vcpkgWarning: '',
  },
  database: {
    reachable: true,
    name: 'ores_dev_brave_hopper',
    restoredAt: '2026-09-28 15:34',
    restoredAgeSeconds: 162000,
    restoredAge: '1d',
    restoredLevel: 'critical',
    schemaVersion: '0.0.25',
    builtFrom: '5559b012aed',
    builtAt: '2026/09/28 14:19:46',
    driftSeconds: 90000,
    driftLabel: '1d behind HEAD — drifting',
    driftLevel: 'warn',
    bootstrapMode: true,
    warning: '',
  },
  services: {
    total: 24,
    counts: { running: 0, starting: 0, stopped: 24, failed: 0, missing: 0 },
    nats: { unit: 'nats-server-brave_hopper', label: 'nats-server', state: 'stopped', detail: 'inactive' },
    units: [
      {
        unit: 'ores.web.service-brave_hopper',
        service: 'ores.web.service',
        replica: 0,
        label: 'web',
        log: 'ores.web.service.0.log',
        state: 'stopped',
        detail: 'inactive',
      },
    ],
    logDir: '/logs',
  },
  health: { level: 'warn', reasons: ['24 of 24 services are stopped'] },
  remedies: {
    startServices: 'compass services start',
    stopServices: 'compass services stop',
    recreateDatabase: 'compass db recreate -y -k',
    configureEnv: 'compass env configure --preset p -y',
  },
}
