// Work tree facts and journal reading. This is the only module that spawns a
// process or reads a file.

import { execFile } from 'node:child_process'
import { createReadStream, promises as fs } from 'node:fs'
import { join } from 'node:path'
import { parseJournalEntry } from './agile.js'

const GIT_TIMEOUT_MS = 10000
const JOURNAL_TAIL_BYTES = 64 * 1024
const CONCURRENCY = 8

function git(args, cwd) {
  return new Promise((resolve) => {
    execFile(
      'git',
      args,
      { cwd, timeout: GIT_TIMEOUT_MS, maxBuffer: 8 * 1024 * 1024 },
      (error, stdout) => resolve(error ? null : stdout),
    )
  })
}

export function environmentOf(root) {
  const name = String(root).replace(/\/+$/, '').split('/').pop() ?? ''
  return name.startsWith('ores_dev_') ? name.slice('ores_dev_'.length) : name
}

function nameOf(root) {
  return String(root).replace(/\/+$/, '').split('/').pop() ?? ''
}

export async function readWorkTree(root) {
  const [head, status] = await Promise.all([
    git(['rev-parse', '--abbrev-ref', 'HEAD'], root),
    git(['status', '--porcelain'], root),
  ])
  const branch = head === null || head.trim() === 'HEAD' ? '' : head.trim()
  return {
    root,
    name: nameOf(root),
    label: environmentOf(root),
    branch,
    detached: head !== null && branch === '',
    dirty: status !== null && status.trim() !== '',
  }
}

// The work tree list is repository-wide, so it is read once from the selected
// tree and answers both the tree control and the fleet rows.
export async function readWorkTrees(cwd) {
  const list = await git(['worktree', 'list', '--porcelain'], cwd)
  if (list === null) return null
  const roots = []
  for (const block of list.split('\n\n')) {
    const lines = block.split('\n')
    const path = lines.find((line) => line.startsWith('worktree '))?.slice('worktree '.length)
    if (!path || lines.includes('bare')) continue
    roots.push(path)
  }
  const trees = []
  for (let index = 0; index < roots.length; index += CONCURRENCY) {
    const slice = roots.slice(index, index + CONCURRENCY)
    trees.push(...(await Promise.all(slice.map((root) => readWorkTree(root)))))
  }
  return trees
}

// The journal is append-only and long; only the last entry is ever needed, so
// read the file's tail rather than the whole thing.
export async function readLastJournal(root) {
  const path = join(root, '.journal.org')
  let size
  try {
    size = (await fs.stat(path)).size
  } catch {
    return null
  }
  const start = Math.max(0, size - JOURNAL_TAIL_BYTES)
  const chunks = []
  try {
    const stream = createReadStream(path, { start, encoding: 'utf8' })
    for await (const chunk of stream) chunks.push(chunk)
  } catch {
    return null
  }
  const lines = chunks.join('').split('\n')
  if (start > 0) lines.shift()
  let lastStart = -1
  for (let index = lines.length - 1; index >= 0; index -= 1) {
    if (/^\* /.test(lines[index])) {
      lastStart = index
      break
    }
  }
  if (lastStart < 0) return null
  const entry = parseJournalEntry(lines.slice(lastStart).join('\n'))
  return entry.taskId || entry.storyId ? entry : null
}

export async function readJournals(roots) {
  const entries = new Map()
  for (let index = 0; index < roots.length; index += CONCURRENCY) {
    const slice = roots.slice(index, index + CONCURRENCY)
    const found = await Promise.all(slice.map((root) => readLastJournal(root)))
    slice.forEach((root, offset) => entries.set(root, found[offset]))
  }
  return entries
}
