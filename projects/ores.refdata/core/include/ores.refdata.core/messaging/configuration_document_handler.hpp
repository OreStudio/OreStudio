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
#ifndef ORES_REFDATA_CORE_MESSAGING_CONFIGURATION_DOCUMENT_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_CONFIGURATION_DOCUMENT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>
#include <string_view>

namespace ores::refdata::messaging {

/**
 * @brief Serves the configuration documents refdata owns: curve configurations and conventions.
 *
 * Every operation runs under the caller's session, so the rows a save writes
 * belong to the caller's party and a read sees what that party may see.
 */
class ORES_REFDATA_CORE_EXPORT configuration_document_handler {
public:
    configuration_document_handler(ores::nats::service::client& nats,
                                   ores::database::context ctx,
                                   std::optional<ores::security::jwt::jwt_authenticator> verifier);

    void save_curve_configuration_document(ores::nats::message msg);
    void get_curve_configuration_document(ores::nats::message msg);
    void delete_curve_configuration_document(ores::nats::message msg);
    void save_conventions_document(ores::nats::message msg);
    void get_conventions_document(ores::nats::message msg);

private:
    /**
     * @brief The caller's context when the caller holds @p permission;
     * otherwise replies with the error and returns nothing.
     */
    std::optional<ores::database::context> authorise(const ores::nats::message& msg,
                                                     std::string_view permission);

    inline static std::string_view logger_name =
        "ores.refdata.messaging.configuration_document_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
