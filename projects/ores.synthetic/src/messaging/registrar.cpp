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
#include "ores.synthetic/messaging/registrar.hpp"

#include <memory>
#include <optional>
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic/messaging/organisation_handler.hpp"

namespace ores::synthetic::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // ----------------------------------------------------------------
    // Organisation
    // ----------------------------------------------------------------
    auto oh = std::make_shared<organisation_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        generate_organisation_request::nats_subject, "ores.synthetic.service",
        [oh](ores::nats::message msg) { oh->generate(std::move(msg)); }));

    return subs;
}

} // namespace ores::synthetic::messaging
