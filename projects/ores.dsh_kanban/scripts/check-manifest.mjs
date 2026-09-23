/**
 * Checks the plugin wiring DSH resolves by name: the manifest fields the loader
 * reads, the export paths it follows, and the client bundle's loader call. Each
 * of these fails the browser roster at runtime with no compile-time signal, so
 * they are asserted here instead.
 *
 * Run from the plugin directory: node scripts/check-manifest.mjs
 */
import { existsSync, readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const failures = []

const specifier = (value) => (typeof value === 'string' ? value : value?.default)
const tryRead = (path) => {
  if (typeof path !== 'string') return null
  const full = resolve(root, path)
  return existsSync(full) ? readFileSync(full, 'utf8') : null
}

const manifestText = tryRead('package.json')
if (manifestText === null) {
  console.error('FAIL package.json is missing')
  process.exit(1)
}
const pkg = JSON.parse(manifestText)
const name = pkg.name
if (typeof name !== 'string' || name === '') {
  console.error('FAIL package.json declares no name')
  process.exit(1)
}

const hostPath = specifier(pkg.main)
const clientPath = specifier(pkg.exports?.['./client'])
for (const [label, path] of [
  ['main', hostPath],
  ['exports["."]', specifier(pkg.exports?.['.'])],
  ['exports["./client"]', clientPath],
]) {
  if (typeof path !== 'string') failures.push(`${label} is not a path`)
  else if (!existsSync(resolve(root, path))) failures.push(`${label} points at ${path}, which does not exist`)
}

if (pkg.dsh?.bundle?.patch !== './cordis.patch.yml') failures.push('dsh.bundle.patch must be ./cordis.patch.yml')
if (pkg.dsh?.client?.platform !== 'web') failures.push('dsh.client.platform must be "web"')

/* The plugin ships with the product, so it carries the product version and is
 * upgraded when that version changes. Enforced rather than trusted, because a
 * version bump that misses this file still installs cleanly and looks fine. */
const cmake = tryRead('../../CMakeLists.txt')
const product = cmake === null
  ? undefined
  : /project\(\s*OreStudio\s+VERSION\s+([0-9][0-9.]*)/.exec(cmake)?.[1]
if (product === undefined) failures.push('cannot read the ORE Studio version from CMakeLists.txt')
else if (pkg.version !== product) {
  failures.push(`package.json version is ${pkg.version}, the product version is ${product}`)
}

const patch = tryRead('cordis.patch.yml')
if (patch === null) failures.push('cordis.patch.yml is missing')
else if (!new RegExp(`name:\\s*['"]${name}['"]`).test(patch)) {
  failures.push(`cordis.patch.yml inserts no row named ${name}`)
}

if (hostPath !== null && existsSync(resolve(root, hostPath))) {
  const host = await import(resolve(root, hostPath))
  if (host.name !== name) failures.push(`host exports name ${host.name}, expected ${name}`)
  if (!Array.isArray(host.inject)) failures.push('host inject is not an array')
  else {
    for (const service of ['webServer', 'sessions']) {
      if (!host.inject.includes(service)) failures.push(`host inject omits ${service}, which it reads`)
    }
  }
  if (typeof host.apply !== 'function') failures.push('host apply is not a function')

  const route = `/plugins/${name}/`
  if (!(tryRead(hostPath) ?? '').includes(route)) failures.push(`host registers no route under ${route}`)
  const client = tryRead(clientPath)
  if (client === null) failures.push('the client bundle is unreadable')
  else {
    if (!/window\.__ModuleLoader__\.load\(\{/.test(client.slice(0, 2000))) {
      failures.push('the client bundle does not open a window.__ModuleLoader__.load call')
    }
    if (!new RegExp(`id:\\s*['"]${name}['"]`).test(client)) {
      failures.push(`the client bundle does not register under the package name ${name}`)
    }
    if (!client.includes(route)) failures.push(`the client fetches no route under ${route}`)
  }
}

if (failures.length) {
  for (const failure of failures) console.error(`FAIL ${failure}`)
  process.exit(1)
}
console.log(`OK ${name}: manifest, patch, host exports, and client bundle wiring agree`)
