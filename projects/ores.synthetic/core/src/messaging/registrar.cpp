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
#include "ores.synthetic.core/messaging/registrar.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.synthetic.core/messaging/folder_registrar.hpp"
#include "ores.synthetic.core/messaging/fx_spot_generation_config_handler.hpp"
#include "ores.synthetic.core/messaging/gmm_component_handler.hpp"
#include "ores.synthetic.core/messaging/ir_curve_generation_config_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_template_entry_registrar.hpp"
#include "ores.synthetic.core/messaging/market_data_generation_config_handler.hpp"
#include "ores.synthetic.core/messaging/organisation_handler.hpp"
#include "ores.synthetic.core/messaging/publish_from_dq_handler.hpp"
#include <memory>
#include <optional>

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
    subs.push_back(
        nats.queue_subscribe(generate_organisation_request::nats_subject,
                             "ores.synthetic.service",
                             [oh](ores::nats::message msg) { oh->generate(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Market data generation config
    // ----------------------------------------------------------------
    {
        constexpr auto queue_group = "ores.synthetic.service";
        auto h = std::make_shared<market_data_generation_config_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            std::string(get_market_data_generation_configs_request::nats_subject),
            queue_group,
            [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            std::string(save_market_data_generation_config_request::nats_subject),
            queue_group,
            [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            std::string(delete_market_data_generation_config_request::nats_subject),
            queue_group,
            [h](ores::nats::message msg) { h->remove(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // FX spot generation config
    // ----------------------------------------------------------------
    {
        constexpr auto queue_group = "ores.synthetic.service";
        auto h = std::make_shared<fx_spot_generation_config_handler>(nats, ctx, verifier);
        subs.push_back(
            nats.queue_subscribe(std::string(get_fx_spot_generation_configs_request::nats_subject),
                                 queue_group,
                                 [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(
            nats.queue_subscribe(std::string(save_fx_spot_generation_config_request::nats_subject),
                                 queue_group,
                                 [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            std::string(delete_fx_spot_generation_config_request::nats_subject),
            queue_group,
            [h](ores::nats::message msg) { h->remove(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // GMM component
    // ----------------------------------------------------------------
    {
        constexpr auto queue_group = "ores.synthetic.service";
        auto h = std::make_shared<gmm_component_handler>(nats, ctx, verifier);
        subs.push_back(
            nats.queue_subscribe(std::string(get_gmm_components_request::nats_subject),
                                 queue_group,
                                 [h](ores::nats::message msg) { h->list(std::move(msg)); }));
        subs.push_back(
            nats.queue_subscribe(std::string(save_gmm_component_request::nats_subject),
                                 queue_group,
                                 [h](ores::nats::message msg) { h->save(std::move(msg)); }));
        subs.push_back(
            nats.queue_subscribe(std::string(delete_gmm_component_request::nats_subject),
                                 queue_group,
                                 [h](ores::nats::message msg) { h->remove(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // IR curve generation config + template entries (generated
    // sub-registrars, unlike the hand-wired entities above).
    // ----------------------------------------------------------------
    {
        auto s = register_ir_curve_generation_config_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
    {
        auto s = register_ir_curve_template_entry_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
    {
        auto s = register_folder_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }

    // ----------------------------------------------------------------
    // Publish-from-DQ workflow step handler
    // ----------------------------------------------------------------
    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        subs.push_back(
            nats.queue_subscribe("synthetic.v1.fx-spot-configs.publish-from-dq",
                                 "ores.synthetic.service",
                                 [pdq](ores::nats::message msg) { pdq->handle(std::move(msg)); }));
    }

    return subs;
}

} // namespace ores::synthetic::messaging
