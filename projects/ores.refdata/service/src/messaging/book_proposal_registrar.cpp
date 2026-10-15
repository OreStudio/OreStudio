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
#include "ores.refdata.service/messaging/book_proposal_registrar.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include "ores.refdata.api/messaging/book_proposal_operations_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_proposal_operations_protocol.hpp"
#include "ores.refdata.api/workflow/book_apply_workflow.hpp"
#include "ores.refdata.api/workflow/counterparty_apply_workflow.hpp"
#include "ores.refdata.service/messaging/book_apply_step_handler.hpp"
#include "ores.refdata.service/messaging/book_proposal_handler.hpp"
#include "ores.refdata.service/messaging/counterparty_apply_step_handler.hpp"
#include "ores.refdata.service/messaging/counterparty_proposal_handler.hpp"
#include <array>
#include <memory>
#include <string_view>
#include <utility>

namespace ores::refdata::messaging {

namespace {

constexpr std::string_view queue_group = "ores.refdata.service";

}

std::vector<ores::nats::service::subscription> book_proposal_registrar::register_handlers(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    // The apply workflow's commands come from the engine, which has proved
    // them, so the step handler takes no verifier.
    // These are workflow step commands, which the engine commits to a stream,
    // so this service declares the coverage and reads them durably.
    static constexpr std::array step_subjects{
        workflow::recheck_book_changes_subject,
        workflow::apply_book_changes_subject,
        workflow::recheck_counterparty_changes_subject,
        workflow::apply_counterparty_changes_subject};
    const auto step_stream =
        ores::service::messaging::ensure_step_stream(nats, queue_group, step_subjects, "apply");

    auto steps = std::make_shared<book_apply_step_handler>(nats, ctx);
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        workflow::recheck_book_changes_subject,
        queue_group,
        step_stream,
        [steps](ores::nats::message msg) { steps->recheck(std::move(msg)); }));
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        workflow::apply_book_changes_subject,
        queue_group,
        step_stream,
        [steps](ores::nats::message msg) { steps->apply(std::move(msg)); }));

    auto cp_steps = std::make_shared<counterparty_apply_step_handler>(nats, ctx);
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        workflow::recheck_counterparty_changes_subject,
        queue_group,
        step_stream,
        [cp_steps](ores::nats::message msg) { cp_steps->recheck(std::move(msg)); }));
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        workflow::apply_counterparty_changes_subject,
        queue_group,
        step_stream,
        [cp_steps](ores::nats::message msg) { cp_steps->apply(std::move(msg)); }));

    auto cp = std::make_shared<counterparty_proposal_handler>(nats, ctx, verifier);
    subs.push_back(
        nats.queue_subscribe(preview_counterparty_changes_request::nats_subject,
                             queue_group,
                             [cp](ores::nats::message msg) { cp->preview(std::move(msg)); }));
    subs.push_back(
        nats.queue_subscribe(raise_counterparty_changes_request::nats_subject,
                             queue_group,
                             [cp](ores::nats::message msg) { cp->raise(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_counterparty_changes_request::nats_subject, queue_group, [cp](ores::nats::message msg) {
            cp->get_changes(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(recheck_counterparty_changes_request::nats_subject,
                             queue_group,
                             [cp](ores::nats::message msg) { cp->recheck(std::move(msg)); }));

    auto h = std::make_shared<book_proposal_handler>(nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        preview_book_changes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->preview(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        raise_book_changes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->raise(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_book_changes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->get_changes(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        recheck_book_changes_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->recheck(std::move(msg));
        }));
    return subs;
}

}
