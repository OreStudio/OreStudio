/**
 * Installs the plugin into a DSH profile as an independent copy.
 *
 * The profile receives its own frozen copy, extracted from a packed tarball, so
 * it keeps working when this work tree changes branch, is reset, or is deleted.
 * The tarball is kept under $DSH_HOME/plugins as the versioned artefact, so the
 * profile's file: reference stays resolvable for later pnpm runs, and so a
 * version can be reinstalled or rolled back without rebuilding it.
 *
 * Re-run after the version changes to upgrade. The version is the ORE Studio
 * version, enforced against CMakeLists.txt by scripts/check-manifest.mjs.
 *
 * Run: node projects/ores.dsh_kanban/scripts/install.mjs [profile]
 */
import { execFileSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const pluginDir = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const profile = process.argv[2] ?? 'web'
const dshHome = process.env['DSH_HOME'] ?? join(homedir(), '.dsh')
const artifactDir = join(dshHome, 'plugins')
const dshBin = process.env['DSH_BIN']
  ?? '/home/marco/.npm-global/lib/node_modules/@deepseek-ai/dsh/lib/bin.js'
const { name, version } = JSON.parse(readFileSync(join(pluginDir, 'package.json'), 'utf8'))

const run = (command, args, options = {}) => execFileSync(command, args, { stdio: 'inherit', ...options })
const dsh = (args) => run(process.execPath, [dshBin, ...args])

run('pnpm', ['pack', '--pack-destination', artifactDir], { cwd: pluginDir })
const tarball = join(artifactDir, `${name}-${version}.tgz`)

try {
  dsh(['plugin', '--profile', profile, 'remove', name])
} catch {
  // A first install has nothing to remove, and pnpm treats that as an error.
}

dsh(['plugin', '--profile', profile, 'add', `file:${tarball}`])

console.log(`\nartefact: ${tarball}`)
console.log(`installed ${name} ${version} into the ${profile} profile as an independent copy`)
console.log('restart the profile to load it. Older artefacts under that directory are safe to delete.')
