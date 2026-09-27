/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief One report definition offered as a template.
 */
export interface DqReportDefinitionTemplate {
    /**
     * @brief The template's name.
     */
    name: string;
    /**
     * @brief What the template produces.
     */
    description: string;
    /**
     * @brief The kind of report the template instantiates.
     */
    report_type: string;
    /**
     * @brief The schedule the template suggests.
     */
    schedule_expression: string;
    /**
     * @brief How overlapping runs of the template are treated.
     */
    concurrency_policy: string;
    /**
     * @brief Where the template sits in the list.
     */
    display_order: number;
}

/**
 * @brief Asks for the templates one dataset bundle offers.
 */
export interface ListDqReportDefinitionTemplatesRequest {
    /**
     * @brief The bundle whose templates are listed.
     */
    bundle_code: string;
}

/**
 * @brief Reports the templates, or why they could not be read.
 */
export interface ListDqReportDefinitionTemplatesResponse {
    /**
     * @brief Whether the read completed.
     */
    success: boolean;
    /**
     * @brief Why it failed, when it did.
     */
    message: string;
    /**
     * @brief The templates, in display order.
     */
    templates: DqReportDefinitionTemplate[];
}

export const subjects = {
    list_dq_report_definition_templates_request: "dq.v1.report-definition-templates.list",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_dq_report_definition_templates_request: true,
} as const;
