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
#include "ores.variability.core/messaging/registrar.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include "ores.variability.core/messaging/operations_handler.hpp"
#include "ores.variability.core/messaging/system_setting_registrar.hpp"
#include <iterator>
#include <memory>
#include <utility>

namespace ores::variability::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // The generated registrar wires the entity surface the model enables.
    // subscription is move-only, so its vector folds in with move iterators.
    const auto fold = [&subs](std::vector<ores::nats::service::subscription> s) {
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    };
    fold(register_system_setting_handlers(nats, ctx, verifier));

    // The component's own operations, which are not entity verbs: clearing a
    // tenant's bootstrap window, and completing one party's onboarding.
    {
        auto ops = std::make_shared<operations_handler>(nats, std::move(ctx), verifier);
        subs.push_back(nats.queue_subscribe(
            std::string(clear_bootstrap_mode_request::nats_subject),
            "ores.variability.service",
            [ops](ores::nats::message msg) { ops->clear_bootstrap_mode(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            std::string(complete_party_onboarding_request::nats_subject),
            "ores.variability.service",
            [ops](ores::nats::message msg) { ops->complete_party_onboarding(std::move(msg)); }));
    }

    return subs;
}

}
