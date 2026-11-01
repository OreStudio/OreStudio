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
#include "ores.reporting.service/messaging/cut_take_registrar.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.service/messaging/cut_take_handler.hpp"
#include <memory>
#include <string_view>
#include <utility>

namespace ores::reporting::messaging {

namespace {

/*
 * The queue the reporting service competes on. It is named here and not shared
 * with the proposal registrar's own constant, because a unity build puts both
 * files in one translation unit and one name would be two definitions.
 */
constexpr std::string_view cut_take_queue_group = "ores.reporting.service";

}

std::vector<ores::nats::service::subscription> cut_take_registrar::register_handlers(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    auto h = std::make_shared<cut_take_handler>(nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        take_cut_request::nats_subject, cut_take_queue_group, [h](ores::nats::message msg) {
            h->take_cut(std::move(msg));
        }));

    return subs;
}

}
