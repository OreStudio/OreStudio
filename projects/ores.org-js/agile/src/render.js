/*
 * Org → HTML rendering for drawer content.
 *
 * Uses the same parser family as org-roam-ui (uniorg) so org syntax is
 * handled properly — tables, lists, emphasis, blocks — instead of the
 * hand-rolled approximation. The output is styled by our dashboard CSS
 * and id: links are intercepted by the app for in-app navigation.
 */
import { unified } from 'https://esm.sh/unified@11.0.5';
import uniorgParse from 'https://esm.sh/uniorg-parse@3.0.1';
import uniorg2rehype from 'https://esm.sh/uniorg-rehype@2.2.0';
import rehypeStringify from 'https://esm.sh/rehype-stringify@10.0.1';

const processor = unified()
  .use(uniorgParse)
  .use(uniorg2rehype)
  .use(rehypeStringify);

const GITHUB_REPO = 'https://github.com/OreStudio/OreStudio';

/**
 * Linkify bare PR/issue references (#1234) in the rendered HTML.
 * Uses /issues/NNN, which GitHub redirects to the PR when NNN is a PR,
 * so both kinds of reference resolve (unlike /pull/NNN for an issue).
 * Operates only on text between tags so attributes and existing
 * anchors are untouched.
 */
function linkifyPrRefs(html) {
  return html.replace(/(^|>)([^<]*)/g, (m, lead, text) =>
    lead + text.replace(
      /#(\d{3,6})\b/g,
      `<a href="${GITHUB_REPO}/issues/$1" target="_blank" rel="noopener">#$1</a>`));
}

/**
 * Strip the file-level :PROPERTIES: drawer and #+keyword header — the
 * drawer header already shows title/description/state — then render
 * the remaining org source to an HTML string. Bare #NNN references
 * become GitHub PR links.
 */
export function orgToHtml(src) {
  let s = src;
  s = s.replace(/^:PROPERTIES:\n(?:.*\n)*?:END:\n/, '');
  s = s.replace(/^#\+[A-Za-z_]+:.*\n/gm, '');
  let html = String(processor.processSync(s));
  html = html.replace(/"proj:/g, '"/OreStudio/');
  return linkifyPrRefs(html);
}
