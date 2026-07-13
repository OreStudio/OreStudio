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
#ifndef ORES_REFDATA_CORE_MESSAGING_HISTORY_CALLER_CONTEXT_HPP
#define ORES_REFDATA_CORE_MESSAGING_HISTORY_CALLER_CONTEXT_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>

namespace ores::refdata::messaging {

/**
 * @brief Packs a fully-resolved request context — tenant, party,
 * visible parties, actor, roles, workspace — into the opaque
 * caller_context string every history.v1.get history_provider in
 * this component receives.
 *
 * Every per-entity history_provider registered on the shared
 * dispatch_registry (see registrar.cpp) must round-trip through this
 * pair, not just forward tenant_id: an entity with party-level row
 * visibility silently loses that filtering otherwise.
 */
ORES_REFDATA_CORE_EXPORT std::string pack_history_caller_context(const ores::database::context& ctx);

/**
 * @brief Reconstructs a request-scoped context from a caller_context
 * string produced by pack_history_caller_context, rooted at @p base_ctx
 * (the service-level context supplying the connection pool/service
 * account). Throws std::runtime_error if caller_context is malformed.
 */
ORES_REFDATA_CORE_EXPORT ores::database::context
unpack_history_caller_context(const ores::database::context& base_ctx, const std::string& caller_context);

}

#endif
