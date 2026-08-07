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
#include "ores.analytics.core/messaging/registrar.hpp"
#include "ores.analytics.core/messaging/pricing_engine_type_registrar.hpp"
#include "ores.analytics.core/messaging/pricing_model_config_registrar.hpp"
#include "ores.analytics.core/messaging/pricing_model_product_registrar.hpp"
#include "ores.analytics.core/messaging/pricing_model_product_parameter_registrar.hpp"

// Generic history.v1.get subject.
#include "ores.history.api/service/version_builder.hpp"
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.analytics.api/domain/pricing_engine_type.hpp"
#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.core/presentation/pricing_engine_type_history_field_mapper.hpp"
#include "ores.analytics.core/presentation/pricing_model_config_history_field_mapper.hpp"
#include "ores.analytics.core/presentation/pricing_model_product_history_field_mapper.hpp"
#include "ores.analytics.core/presentation/pricing_model_product_parameter_history_field_mapper.hpp"
#include "ores.analytics.core/service/pricing_engine_type_service.hpp"
#include "ores.analytics.core/service/pricing_model_config_service.hpp"
#include "ores.analytics.core/service/pricing_model_product_service.hpp"
#include "ores.analytics.core/service/pricing_model_product_parameter_service.hpp"

// Function-local static: must outlive the history.v1.get subscription.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

namespace ores::analytics::messaging {

namespace {

template <typename T>
void append(std::vector<T>& dst, std::vector<T>&& src) {
    dst.insert(dst.end(),
               std::make_move_iterator(src.begin()),
               std::make_move_iterator(src.end()));
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // Per-entity handler subscriptions (codegen-generated).
    append(subs, register_pricing_engine_type_handlers(nats, ctx, verifier));
    append(subs, register_pricing_model_config_handlers(nats, ctx, verifier));
    append(subs, register_pricing_model_product_handlers(nats, ctx, verifier));
    append(subs, register_pricing_model_product_parameter_handlers(nats, ctx, verifier));

    // Generic history.v1.get subject and provider registrations.
    {
        auto& hist_registry = history_registry();
        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "analytics", "ores.analytics.service", ctx, verifier));

        hist_registry.register_history_provider(
            "ores.analytics.pricing_engine_type",
            [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
                service::pricing_engine_type_service svc(scoped_ctx);
                auto versions = svc.get_type_history(entity_id);
                return ores::history::service::build_entity_history_versions(
                    versions, presentation::render_pricing_engine_type_fields);
            });

        hist_registry.register_history_provider(
            "ores.analytics.pricing_model_config",
            [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
                service::pricing_model_config_service svc(scoped_ctx);
                auto versions = svc.get_config_history(entity_id);
                return ores::history::service::build_entity_history_versions(
                    versions, presentation::render_pricing_model_config_fields);
            });

        hist_registry.register_history_provider(
            "ores.analytics.pricing_model_product",
            [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
                service::pricing_model_product_service svc(scoped_ctx);
                auto versions = svc.get_product_history(entity_id);
                return ores::history::service::build_entity_history_versions(
                    versions, presentation::render_pricing_model_product_fields);
            });

        hist_registry.register_history_provider(
            "ores.analytics.pricing_model_product_parameter",
            [](const ores::database::context& scoped_ctx, const std::string& entity_id) {
                service::pricing_model_product_parameter_service svc(scoped_ctx);
                auto versions = svc.get_parameter_history(entity_id);
                return ores::history::service::build_entity_history_versions(
                    versions, presentation::render_pricing_model_product_parameter_fields);
            });
    }

    return subs;
}

}
