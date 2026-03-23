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
#include "ores.reporting.core/messaging/registrar.hpp"

#include <memory>
#include <optional>
#include "ores.reporting.api/messaging/report_type_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting.core/messaging/report_type_handler.hpp"
#include "ores.reporting.core/messaging/report_definition_handler.hpp"
#include "ores.reporting.core/messaging/report_instance_handler.hpp"
#include "ores.reporting.core/messaging/concurrency_policy_handler.hpp"

namespace ores::reporting::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // ----------------------------------------------------------------
    // Report types
    // ----------------------------------------------------------------
    auto rth = std::make_shared<report_type_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        get_report_types_request::nats_subject, "ores.reporting.service",
        [rth](ores::nats::message msg) { rth->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_report_type_request::nats_subject, "ores.reporting.service",
        [rth](ores::nats::message msg) { rth->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_report_type_request::nats_subject, "ores.reporting.service",
        [rth](ores::nats::message msg) { rth->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_report_type_history_request::nats_subject, "ores.reporting.service",
        [rth](ores::nats::message msg) { rth->history(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Report definitions
    // ----------------------------------------------------------------
    auto rdh = std::make_shared<report_definition_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        get_report_definitions_request::nats_subject, "ores.reporting.service",
        [rdh](ores::nats::message msg) { rdh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_report_definition_request::nats_subject, "ores.reporting.service",
        [rdh](ores::nats::message msg) { rdh->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_report_definition_request::nats_subject, "ores.reporting.service",
        [rdh](ores::nats::message msg) { rdh->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_report_definition_history_request::nats_subject, "ores.reporting.service",
        [rdh](ores::nats::message msg) { rdh->history(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        schedule_report_definitions_request::nats_subject, "ores.reporting.service",
        [rdh](ores::nats::message msg) { rdh->schedule(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        unschedule_report_definitions_request::nats_subject, "ores.reporting.service",
        [rdh](ores::nats::message msg) { rdh->unschedule(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Report instances
    // ----------------------------------------------------------------
    auto rih = std::make_shared<report_instance_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        get_report_instances_request::nats_subject, "ores.reporting.service",
        [rih](ores::nats::message msg) { rih->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_report_instance_request::nats_subject, "ores.reporting.service",
        [rih](ores::nats::message msg) { rih->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_report_instance_request::nats_subject, "ores.reporting.service",
        [rih](ores::nats::message msg) { rih->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_report_instance_history_request::nats_subject, "ores.reporting.service",
        [rih](ores::nats::message msg) { rih->history(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Concurrency policies
    // ----------------------------------------------------------------
    auto cph = std::make_shared<concurrency_policy_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        get_concurrency_policies_request::nats_subject, "ores.reporting.service",
        [cph](ores::nats::message msg) { cph->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_concurrency_policy_request::nats_subject, "ores.reporting.service",
        [cph](ores::nats::message msg) { cph->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_concurrency_policy_request::nats_subject, "ores.reporting.service",
        [cph](ores::nats::message msg) { cph->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_concurrency_policy_history_request::nats_subject, "ores.reporting.service",
        [cph](ores::nats::message msg) { cph->history(std::move(msg)); }));

    return subs;
}

} // namespace ores::reporting::messaging
