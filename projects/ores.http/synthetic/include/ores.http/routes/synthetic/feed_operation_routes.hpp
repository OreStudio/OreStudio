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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_FEED_OPERATION_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_FEED_OPERATION_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/messaging/feed_overview_protocol.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <memory>
#include <type_traits>
#include <vector>

namespace ores::http::routes::synthetic {

/**
 * @brief The synthetic folder tree: the children of one folder, or the roots,
 * with the feeds under each at any depth and how many of each kind, so no
 * screen reads the feeds.
 */
struct feed_folder_tree final {
    std::vector<ores::synthetic::messaging::feed_folder> folders;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_folder_tree>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"folders", &feed_folder_tree::folders}};
}

/**
 * @brief The feed screen's operations: one feed or one folder's feeds started
 * and stopped, a page of the feeds a filter matches with whether each runs, and
 * the start and stop of a selection, and the folder tree view.
 *
 * A start or stop the service refuses conflicts with the feed's state, so it
 * answers 409 with the service's words. A refused page or selection is a bad
 * filter, so it answers 400.
 */
class feed_operation_routes {
public:
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::feed_folder_tree>
    : shaped_wire_traits<ores::http::routes::synthetic::feed_folder_tree> {};

#endif
