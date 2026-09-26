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
#include "ores.compute.core/messaging/workunit_registrar.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.compute.core/messaging/workunit_handler.hpp"
#include <memory>

namespace ores::compute::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.compute.service";
} // namespace

std::vector<ores::nats::service::subscription>
register_workunit_handlers(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<workunit_handler>(nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        list_workunits_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->list_workunits(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_workunit_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_workunit(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_workunits_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_many_workunits(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_workunit_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->put_workunit(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_workunits_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->put_many_workunits(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_workunit_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->delete_workunit(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_workunits_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->delete_many_workunits(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_by_batch_id_workunits_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->list_by_batch_id_workunits(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        list_workunit_versions_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->list_workunit_versions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_workunit_version_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_workunit_version(std::move(msg));
        }));
    return subs;
}

} // namespace ores::compute::messaging
