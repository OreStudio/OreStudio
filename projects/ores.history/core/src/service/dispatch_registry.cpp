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
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <exception>

namespace ores::history::service {

void dispatch_registry::register_history_provider(std::string entity_type,
                                                  std::string read_permission,
                                                  history_provider provider) {
    providers_[std::move(entity_type)] = {.read_permission = std::move(read_permission),
                                          .provider = std::move(provider)};
}

bool dispatch_registry::has_provider(const std::string& entity_type) const {
    return providers_.contains(entity_type);
}

std::size_t dispatch_registry::provider_count() const {
    return providers_.size();
}

messaging::get_entity_history_response
dispatch_registry::dispatch(const messaging::get_entity_history_request& request,
                            const ores::database::context& ctx) const {
    const auto it = providers_.find(request.entity_type);
    if (it == providers_.end()) {
        return {.versions = {},
                .success = false,
                .message =
                    "No history provider registered for entity_type: " + request.entity_type};
    }

    const auto& registered = it->second;
    if (!registered.read_permission.empty() &&
        !ores::service::messaging::has_permission(ctx, registered.read_permission)) {
        return {.versions = {},
                .success = false,
                .message = "Reading this history needs " + registered.read_permission + "."};
    }

    try {
        return {.versions = registered.provider(ctx, request.entity_id),
                .success = true,
                .message = {}};
    } catch (const std::exception& e) {
        return {.versions = {},
                .success = false,
                .message = "History provider for entity_type '" + request.entity_type +
                           "' failed: " + e.what()};
    }
}

}
