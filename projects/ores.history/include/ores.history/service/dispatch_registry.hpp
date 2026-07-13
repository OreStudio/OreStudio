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
#ifndef ORES_HISTORY_SERVICE_DISPATCH_REGISTRY_HPP
#define ORES_HISTORY_SERVICE_DISPATCH_REGISTRY_HPP

#include "ores.history/export.hpp"
#include "ores.history/messaging/history_protocol.hpp"
#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::history::service {

/**
 * @brief A history provider: fetches and renders one entity's version
 * history, given its id and the caller's scope. Generated, mechanical
 * glue per entity — call the entity's own repository/service, then
 * its field mapper.
 *
 * caller_context is an opaque string this leaf component neither
 * produces nor interprets: whatever a caller-supplied resolver (see
 * messaging::register_history_handlers) packs into it — typically
 * tenant_id, and where relevant party_id/actor for row-level
 * visibility — a provider unpacks in whatever shape it and the
 * resolver privately agree on (e.g. "tenant_id|party_id"). Keeping it
 * opaque here is what lets ores.history stay a dependency-free leaf
 * with no ores.database/ores.security dependency.
 */
using history_provider = std::function<std::vector<messaging::entity_history_version>(
    const std::string& caller_context, const std::string& entity_id)>;

/**
 * @brief Server-side dispatch table for the one generic history
 * subject: looks up a caller-registered history_provider by
 * entity_type and delegates to it.
 *
 * Reapplies the entity-composed-registrars meta-pattern as a runtime
 * lookup table instead of N separate subject subscriptions. Failure
 * isolation matches that pattern: registering a bad provider affects
 * only its own entity_type entry, not the shared subject.
 */
class ORES_HISTORY_EXPORT dispatch_registry final {
public:
    /**
     * @brief Registers a history provider for entity_type. A second
     * registration for the same entity_type replaces the first.
     */
    void register_history_provider(std::string entity_type, history_provider provider);

    /**
     * @brief True if a provider is registered for entity_type.
     */
    [[nodiscard]] bool has_provider(const std::string& entity_type) const;

    /**
     * @brief Number of registered providers.
     */
    [[nodiscard]] std::size_t provider_count() const;

    /**
     * @brief Dispatches a request to the provider registered for its
     * entity_type, passing through caller_context unexamined. Returns
     * a failure response (success = false) when no provider is
     * registered, or when the registered provider throws, rather than
     * propagating an exception — the caller is a NATS handler that
     * must always reply.
     */
    [[nodiscard]] messaging::get_entity_history_response
    dispatch(const messaging::get_entity_history_request& request,
             const std::string& caller_context) const;

private:
    std::unordered_map<std::string, history_provider> providers_;
};

}

#endif
