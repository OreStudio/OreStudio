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
export interface CompleteTenantProvisioningCommand {
}

export interface CompleteTenantProvisioningResponse {
    success: boolean;
    message: string;
}

// --- Acme one-click tenant provisioning (--source acme) ---
//
// A single server-side orchestrated request: imports the four-party Acme
// Bank LEI hierarchy, publishes real GLEIF counterparties (small), then
// for each operating company publishes its business units, portfolios,
// books, accounts, and account contact informations. No repeated
// per-party logins, no orchestration logic client-side -- driven by
// internal actor impersonation through the real handler pipeline, see
// ores.iam.core/messaging/tenant_provisioning_handler.hpp's provision_acme.
export interface ProvisionAcmeTenantCommand {
}

export interface ProvisionAcmeTenantStep {
    step: string;
    action: string;
    record_count: number;
}

export interface ProvisionAcmeTenantResponse {
    success: boolean;
    message: string;
    steps: ProvisionAcmeTenantStep[];
}

export const subjects = {
    complete_tenant_provisioning_command: "iam.v1.tenants.complete-provisioning",
    provision_acme_tenant_command: "iam.v1.tenants.provision-acme",
} as const;
