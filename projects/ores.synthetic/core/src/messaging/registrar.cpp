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
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.synthetic.core/messaging/folder_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/folder_registrar.hpp"
#include "ores.synthetic.core/messaging/fx_spot_generation_config_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/fx_spot_generation_config_registrar.hpp"
#include "ores.synthetic.core/messaging/gmm_component_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/gmm_component_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_generation_config_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_generation_config_process_parameter_value_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_generation_config_process_parameter_value_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_generation_config_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_template_entry_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/ir_curve_template_entry_registrar.hpp"
#include "ores.synthetic.core/messaging/market_data_generation_config_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/market_data_generation_config_registrar.hpp"
#include "ores.synthetic.core/messaging/organisation_handler.hpp"
#include "ores.synthetic.core/messaging/publish_from_dq_handler.hpp"
#include "ores.synthetic.core/messaging/yield_curve_process_parameter_definition_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/yield_curve_process_parameter_definition_registrar.hpp"
#include "ores.synthetic.core/messaging/yield_curve_process_type_history_provider_registrar.hpp"
#include "ores.synthetic.core/messaging/yield_curve_process_type_registrar.hpp"
#include <memory>
#include <optional>

namespace ores::synthetic::messaging {

namespace {

// Function-local static: must outlive the history.v1.get subscription
// (see ores::history::messaging::register_history_handlers's doc), and
// register_handlers is only ever called once per service process.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

} // namespace

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
    // Market data generation config, FX spot generation config and GMM
    // component (generated sub-registrars: the full subject set per
    // entity, including history).
    // ----------------------------------------------------------------
    {
        auto s = register_market_data_generation_config_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
    {
        auto s = register_fx_spot_generation_config_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
    {
        auto s = register_gmm_component_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }

    // ----------------------------------------------------------------
    // Yield curve process type + parameter definitions + IR curve
    // generation config + template entries (generated sub-registrars,
    // unlike the hand-wired entities above).
    // ----------------------------------------------------------------
    {
        auto s = register_yield_curve_process_type_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
    {
        auto s = register_yield_curve_process_parameter_definition_handlers(nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
    {
        auto s = register_ir_curve_generation_config_process_parameter_value_handlers(
            nats, ctx, verifier);
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    }
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
            nats.queue_subscribe("synthetic.v1.theme.publish-from-dq",
                                 "ores.synthetic.service",
                                 [pdq](ores::nats::message msg) { pdq->handle(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Generic history.v1.get subject. The registrar resolves each
    // request into a scoped context exactly like every other subject,
    // so a provider sees the same tenant/party/roles/workspace
    // visibility any other handler in this file would.
    // ----------------------------------------------------------------
    {
        constexpr auto queue_group = "ores.synthetic.service";
        auto& hist_registry = history_registry();
        // Per-entity history providers (generated by the
        // history-provider-registrar facet).
        register_folder_history_provider(hist_registry);
        register_fx_spot_generation_config_history_provider(hist_registry);
        register_gmm_component_history_provider(hist_registry);
        register_ir_curve_generation_config_history_provider(hist_registry);
        register_ir_curve_generation_config_process_parameter_value_history_provider(hist_registry);
        register_ir_curve_template_entry_history_provider(hist_registry);
        register_market_data_generation_config_history_provider(hist_registry);
        register_yield_curve_process_parameter_definition_history_provider(hist_registry);
        register_yield_curve_process_type_history_provider(hist_registry);
        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "synthetic", queue_group, ctx, verifier));
    }

    return subs;
}

} // namespace ores::synthetic::messaging
