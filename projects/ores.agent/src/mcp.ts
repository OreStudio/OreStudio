// SPDX-License-Identifier: GPL-3.0-or-later
import {
    McpServer,
    createMcpHandler,
    type StandardSchemaWithJSON,
} from '@modelcontextprotocol/server';
import { toNodeHandler } from '@modelcontextprotocol/node';
import { AppError } from './backend.js';
import { tools, type ToolExecutor } from './tools.js';
import type { Sessions } from './auth.js';
import { version } from './version.js';

export function createMcp(sessions: Sessions, executor: ToolExecutor) {
    const handler = createMcpHandler(
        (ctx) => {
            // Re-authenticate every HTTP exchange. Protocol metadata never selects an identity.
            const session = sessions.require(
                ctx.requestInfo?.headers.get('authorization') ?? undefined,
            );
            const server = new McpServer(
                { name: 'ores.agent', version },
                {
                    supportedProtocolVersions: ['2026-07-28', '2025-11-25'],
                },
            );
            for (const [name, tool] of Object.entries(tools)) {
                server.registerTool(
                    name,
                    {
                        description: tool.description,
                        inputSchema: tool.schema as StandardSchemaWithJSON,
                        annotations: {
                            readOnlyHint: true,
                            destructiveHint: false,
                            idempotentHint: true,
                            openWorldHint: false,
                        },
                    },
                    async (args, context) => {
                        try {
                            const result = await executor.execute(
                                name,
                                args,
                                session,
                                context.mcpReq.signal,
                            );
                            return { content: [{ type: 'text', text: JSON.stringify(result) }] };
                        } catch (error) {
                            return {
                                isError: true,
                                content: [
                                    {
                                        type: 'text',
                                        text: JSON.stringify({
                                            error:
                                                error instanceof AppError
                                                    ? error.code
                                                    : 'invalid_tool_request',
                                        }),
                                    },
                                ],
                            };
                        }
                    },
                );
            }
            return server;
        },
        { legacy: 'stateless', responseMode: 'json', maxRequestBodySize: 32768 },
    );
    return { handle: toNodeHandler(handler, { maxRequestBodySize: 32768 }), close: handler.close };
}
