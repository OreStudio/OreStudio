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
#ifndef ORES_ORE_SERVICE_MESSAGING_PRICING_ENGINES_HANDLER_HPP
#define ORES_ORE_SERVICE_MESSAGING_PRICING_ENGINES_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <string>

namespace ores::ore::service::messaging {

/**
 * @brief Serves the ORE pricing engine configuration's document pair: the
 * export that writes pricingengine.xml from one configuration, the preview that
 * says what importing a file would do, and the import that writes it with the
 * person's choices.
 *
 * The configuration is read and written through analytics' own messages under
 * the caller's delegated session, so the party and the permissions are the
 * caller's, and the name of the file is read from the reporting file types table
 * rather than written here. The document itself is read and written by the one
 * importer and the one exporter in ores.ore.core.
 *
 * The import writes through the save the configuration's own screen uses, so
 * the validation, the history and the party rules are the screen's. A
 * configuration the file does not have yet is registered in reporting first and
 * removed again when the save is refused, which is the order the screen's create
 * takes.
 */
class pricing_engines_handler {
private:
    inline static std::string_view logger_name =
        "ores.ore.service.messaging.pricing_engines_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    pricing_engines_handler(ores::nats::service::client& nats,
                            ores::database::context ctx,
                            ores::security::jwt::jwt_authenticator signer,
                            ores::nats::service::nats_client outbound_nats);

    void export_pricing_engines(ores::nats::message msg);

    /**
     * @brief Parses a pricing engines document and says what importing it would
     * do. Writes nothing.
     */
    void preview_pricing_engines_import(ores::nats::message msg);

    /**
     * @brief Writes a pricing engines document with the person's choices.
     */
    void import_pricing_engines(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    ores::nats::service::nats_client outbound_nats_;
};

}

#endif
