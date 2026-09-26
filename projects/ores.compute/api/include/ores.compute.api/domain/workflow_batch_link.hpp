/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_COMPUTE_API_DOMAIN_WORKFLOW_BATCH_LINK_HPP
#define ORES_COMPUTE_API_DOMAIN_WORKFLOW_BATCH_LINK_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::compute::domain {

/**
 * @brief The async bridge between a compute batch and the workflow step that waits for it.
 *
 * When a submit_compute workflow step runs, the service creates a batch and
 * records one row here that names the step and the step's instance. The
 * bridge polls this table, and when the batch reaches a terminal state it
 * publishes step_completed_event and deletes the row.
 *
 * The table has no validity window and carries no audit tail: the row is a
 * note that a step is outstanding, and it is deleted rather than closed. The
 * model therefore states :current_state: and :no_audit_columns:.
 *
 * The one decision the model records is the read scope. The bridge is a
 * service-level poller, not a caller acting for one tenant, so it reads
 * every tenant's links and deletes each under the tenant that owns it. That
 * is why the model states :tenant_read_scope: shared, which leaves the
 * tenant filter to row-level security rather than adding one on top, and
 * :rls_system_tenant_visible:, which is what lets the service's
 * system-tenant session see rows the tenant owns. Mutations stay
 * tenant-scoped, so the delete names the link's own tenant.
 */
struct workflow_batch_link final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The batch the step waits for. One step waits for one batch, so this column is the
     * whole key.
     */
    boost::uuids::uuid batch_id;

    /**
     * @brief The workflow step to complete when the batch finishes.
     */
    std::string workflow_step_id;

    /**
     * @brief The workflow instance the step belongs to.
     */
    std::string workflow_instance_id;

    /**
     * @brief When the link was recorded.
     */
    std::chrono::system_clock::time_point created_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const workflow_batch_link&, const workflow_batch_link&) = default;
};

/**
 * @brief Dispatch-key identifier for workflow_batch_link, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const workflow_batch_link&) {
    return "ores.compute.workflow_batch_link";
}

}

#endif
