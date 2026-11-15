// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { tools } from './tools.js';
import { discoveryTools } from './discovery.js';
import { reportingTools } from './reporting.js';
import { scopeTools } from './scope-reporting.js';
import type { RequestPlan } from './intent.js';
import { analyticsTools } from './fx-analytics.js';
import { marketCurveTools } from './fx-market-curve.js';
import { markdownText } from './markdown.js';
import { pythonTool } from './python-sandbox.js';
import { calculationModelInput } from './calculation.js';

const defineTool = (name: string, spec: { description: string; schema: z.ZodType }) => ({
    type: 'function',
    function: {
        name,
        description: spec.description,
        parameters: z.toJSONSchema(spec.schema, { io: 'input' }),
    },
});
const definitions = Object.entries(tools).map(([name, spec]) =>
    defineTool(name, name === 'calculate' ? { ...spec, schema: calculationModelInput } : spec),
);
const inspectionInput = z
    .object({
        name: z.string().trim().min(1).max(128).optional(),
    })
    .strict();
export const toolInspector = defineTool('inspect_tools', {
    description:
        'Use for questions about the agent’s tools and arguments. “List all tools” → {}; “What does search_help take?” → {"name":"search_help"}. Reads the full registry, including tools outside the current turn. Use search_help for OreStudio workflows.',
    schema: inspectionInput,
});
const chatDefinitions = [...definitions, toolInspector];
const pythonDefinition = defineTool('run_python', pythonTool);
const routes: Record<RequestPlan['route'] | 'help', Set<string>> = {
    help: new Set(['search_help']),
    analytics: new Set([...Object.keys(analyticsTools), ...Object.keys(marketCurveTools)]),
    scope: new Set(Object.keys(scopeTools)),
    reporting: new Set([...Object.keys(reportingTools), 'find_trades']),
    discovery: new Set([...Object.keys(discoveryTools), 'list_currencies']),
    market: new Set(
        Object.keys(tools).filter(
            (name) =>
                name !== 'search_help' &&
                name !== 'calculate' &&
                !Object.hasOwn(discoveryTools, name) &&
                !Object.hasOwn(reportingTools, name) &&
                !Object.hasOwn(scopeTools, name) &&
                !Object.hasOwn(analyticsTools, name) &&
                !Object.hasOwn(marketCurveTools, name),
        ),
    ),
};
export function catalogue(route: keyof typeof routes, computation?: { pythonEnabled: boolean }) {
    const selected = definitions.filter(
        ({ function: tool }) =>
            routes[route].has(tool.name) || (computation && tool.name === 'calculate'),
    );
    if (computation?.pythonEnabled) selected.push(pythonDefinition);
    return selected;
}

export const toolGroups = [
    'discovery',
    'reporting',
    'scope',
    'market',
    'analytics',
    'help',
] as const;
export type ToolGroup = (typeof toolGroups)[number];
export const enableToolsInput = z
    .object({ groups: z.array(z.enum(toolGroups)).min(1).max(6) })
    .strict();
export const enableTools = defineTool('enable_tools', {
    description:
        'Enable additional tools for the next step of this task. Groups: discovery (trade details, books, portfolios, counterparties), reporting (stored report runs and trade results), scope (book/portfolio totals and risk), market (FX/IR quotes, series and observations), analytics (FX curves and volatility surfaces), help (documentation). Use when the next needed tool is absent; for example after finding trades, enable market to read FX spot. This does not execute business operations.',
    schema: enableToolsInput,
});

export function workflowCatalogue(groups: ToolGroup[], pythonEnabled: boolean) {
    const selected = new Map<string, ReturnType<typeof defineTool>>();
    for (const group of groups)
        for (const tool of catalogue(group, { pythonEnabled }))
            selected.set(tool.function.name, tool);
    for (const tool of [toolInspector, enableTools]) selected.set(tool.function.name, tool);
    return [...selected.values()];
}

export function inspectTools(input: unknown, pythonEnabled = false): string {
    const { name } = inspectionInput.parse(input);
    const available = pythonEnabled
        ? [...chatDefinitions, enableTools, pythonDefinition]
        : [...chatDefinitions, enableTools];
    if (!name) {
        return (
            'The agent has these registered tools:\n\n' +
            available.map(({ function: tool }) => '- `' + tool.name + '`').join('\n')
        );
    }
    const match = available.find(({ function: tool }) => tool.name === name.toLowerCase());
    if (!match) return `I don’t have a registered tool named “${markdownText(name)}”.`;
    return [
        `\`${match.function.name}\` is available. ${match.function.description}`,
        '\n```json',
        JSON.stringify(match, null, 2),
        '```',
    ].join('\n');
}
