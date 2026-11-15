// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { marketId } from './market.js';

const launch =
    /^(?:(?:can|could|would)\s+you\s+)?(?:please\s+)?(?:run|start|execute|trigger)\s+(?:(?:a|the|new|an)\s+)*(?:(npv)\s+)?report\b/i;
const thenResults =
    /(?:,?\s+then\s+(?:show|return|give|display)\s+(?:me\s+)?(?:the\s+)?(?:npv\s+)?results)(?:\s+for\s+(book|portfolio|trade)\s+(.+))?$/i;
const launchScope =
    /^(?:(?:using\s+(?:report\s+)?definition\s+("[^"]+"|[^"]+?))\s+)?for\s+(book|portfolio|trade)\s+(.+)$/i;
const useDefinition = /^using\s+(?:report\s+)?definition\s+("[^"]+"|[^"]+)$/i;
const dateAtEnd = /(?:^|\s+)(?:on|at|as of|for)\s+(\d{4}-\d{2}-\d{2})$/i;
export type ScopeSelector = { type: 'book' | 'portfolio' | 'trade'; selector: string };
export type RunRequest = {
    npvOnly: boolean;
    selector?: string;
    date?: string;
    requestedExecutionScope?: ScopeSelector;
    resultScope?: ScopeSelector;
    follow: boolean;
};
const unquote = (text: string) => text.trim().replace(/^"([^"]+)"$/, '$1');
function scope(type: string | undefined, selector: string | undefined): ScopeSelector | undefined {
    if (!type || !selector) return;
    const value = unquote(selector);
    if (!value || value.length > 120 || value.includes('"')) return;
    const normalized = type.toLowerCase() as ScopeSelector['type'];
    if (normalized === 'trade' && !marketId.safeParse(value).success) return;
    return { type: normalized, selector: value };
}

export function reportRunRequest(message: string): RunRequest | 'clarify' | undefined {
    const prefix = launch.exec(message);
    if (!prefix) return;
    let rest = message
        .slice(prefix[0].length)
        .trim()
        .replace(/[.!?]$/, '')
        .trim();
    const follow = thenResults.exec(rest);
    const resultScope = scope(follow?.[1], follow?.[2]);
    if (follow?.[1] && !resultScope) return 'clarify';
    if (follow) rest = rest.slice(0, follow.index).trim();
    const dateMatch = dateAtEnd.exec(rest);
    const date = dateMatch?.[1];
    if (date && !z.iso.date().safeParse(date).success) return 'clarify';
    if (dateMatch) rest = rest.slice(0, dateMatch.index).trim();
    const scoped = launchScope.exec(rest);
    const requestedExecutionScope = scope(scoped?.[2], scoped?.[3]);
    if (scoped && !requestedExecutionScope) return 'clarify';
    const selection = scoped?.[1] ?? useDefinition.exec(rest)?.[1];
    const selector = selection ? unquote(selection) : undefined;
    if (selector && (selector.length > 120 || !selector.trim())) return 'clarify';
    if (rest && !scoped && !selector) return 'clarify';
    return {
        npvOnly: Boolean(prefix[1]),
        selector,
        date,
        requestedExecutionScope,
        resultScope,
        follow: Boolean(follow),
    };
}
