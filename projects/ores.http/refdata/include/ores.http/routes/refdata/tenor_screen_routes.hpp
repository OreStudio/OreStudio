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
#ifndef ORES_HTTP_ROUTES_REFDATA_TENOR_SCREEN_ROUTES_HPP
#define ORES_HTTP_ROUTES_REFDATA_TENOR_SCREEN_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/tenor_codec.hpp"
#include "ores.refdata.api/domain/tenor_convention_codec.hpp"
#include "ores.refdata.api/domain/tenor_schedule_codec.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <memory>
#include <type_traits>
#include <vector>

namespace ores::http::routes::refdata {

/**
 * @brief The tenor catalogue: the tenors in their own order, with the
 * conventions and the schedules that resolve them, because a tenor is
 * meaningless without the rules that resolve it.
 */
struct tenor_catalogue final {
    std::vector<ores::refdata::domain::tenor> tenors;
    std::vector<ores::refdata::domain::tenor_convention> conventions;
    std::vector<ores::refdata::domain::tenor_schedule> schedules;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<tenor_catalogue>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"tenors", &tenor_catalogue::tenors},
                                              field{"conventions", &tenor_catalogue::conventions},
                                              field{"schedules", &tenor_catalogue::schedules}};
}

/**
 * @brief The tenors screen's operations: the tenor catalogue view, the
 * resolution of a drafted rule and
 * of the rules as stored, one convention's membership rows, and the writes of
 * a tenor, a convention and a membership row.
 *
 * A rule the service declines is an answer, not a failure, so a resolution and
 * a write answer 200 with the service's result whatever its outcome.
 */
class tenor_screen_routes {
public:
    /// The catalogue from the three lists, the tenors put in their own order.
    static tenor_catalogue
    catalogue_of(std::vector<ores::refdata::domain::tenor> tenors,
                 std::vector<ores::refdata::domain::tenor_convention> conventions,
                 std::vector<ores::refdata::domain::tenor_schedule> schedules);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::refdata::tenor_catalogue>
    : shaped_wire_traits<ores::http::routes::refdata::tenor_catalogue> {};

#endif
