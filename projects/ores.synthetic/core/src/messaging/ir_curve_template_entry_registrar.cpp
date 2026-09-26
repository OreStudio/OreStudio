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
#include "ores.synthetic.core/messaging/ir_curve_template_entry_registrar.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include "ores.synthetic.core/messaging/ir_curve_template_entry_handler.hpp"
#include <memory>

namespace ores::synthetic::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.synthetic.service";
} // namespace

std::vector<ores::nats::service::subscription> register_ir_curve_template_entry_handlers(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<ir_curve_template_entry_handler>(
        nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        list_ir_curve_template_entries_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->list_ir_curve_template_entries(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_ir_curve_template_entry_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->get_ir_curve_template_entry(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_many_ir_curve_template_entries_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->get_many_ir_curve_template_entries(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        put_ir_curve_template_entry_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->put_ir_curve_template_entry(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        put_many_ir_curve_template_entries_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->put_many_ir_curve_template_entries(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_ir_curve_template_entry_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->delete_ir_curve_template_entry(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(delete_many_ir_curve_template_entries_request::nats_subject,
                                        queue_group,
                                        [h](ores::nats::message msg) {
                                            h->delete_many_ir_curve_template_entries(
                                                std::move(msg));
                                        }));
    subs.push_back(nats.queue_subscribe(list_ir_curve_template_entry_versions_request::nats_subject,
                                        queue_group,
                                        [h](ores::nats::message msg) {
                                            h->list_ir_curve_template_entry_versions(
                                                std::move(msg));
                                        }));
    subs.push_back(nats.queue_subscribe(
        get_ir_curve_template_entry_version_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->get_ir_curve_template_entry_version(std::move(msg)); }));
    return subs;
}

} // namespace ores::synthetic::messaging
