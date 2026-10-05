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
 * Template: cpp_nats_registrar.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.dq.core/messaging/coding_scheme_registrar.hpp"
#include "ores.dq.api/messaging/coding_scheme_protocol.hpp"
#include "ores.dq.core/messaging/coding_scheme_handler.hpp"
#include <memory>

namespace ores::dq::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.dq.service";
}

std::vector<ores::nats::service::subscription>
register_coding_scheme_handlers(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<coding_scheme_handler>(nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        list_coding_schemes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->list_coding_schemes(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_coding_scheme(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_coding_schemes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_many_coding_schemes(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_coding_scheme_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->put_coding_scheme(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_coding_schemes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->put_many_coding_schemes(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_coding_scheme_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->delete_coding_scheme(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_coding_schemes_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->delete_many_coding_schemes(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        list_coding_scheme_versions_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->list_coding_scheme_versions(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_coding_scheme_version_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_coding_scheme_version(std::move(msg));
        }));
    return subs;
}

}
