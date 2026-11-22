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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPT_PUBLISH_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPT_PUBLISH_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/messaging/script_library_operation_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include <string_view>

namespace ores::trading::service {

/**
 * @brief Publishes a tenant's own script as its next version.
 *
 * Each variant is parsed with ORE's grammar, and a script whose parse reports
 * an error is refused with the diagnostics and writes nothing. The inputs are
 * derived from the variants rather than stated by the caller. The script and
 * every row beneath it are written in one transaction, each child row carrying
 * the new version as its script version, and a row the new version no longer
 * has is closed. A script equal to its current version writes nothing and
 * keeps that version.
 *
 * The world library is held by the system tenant and is published from its
 * dataset, so a publish in the system tenant is refused.
 */
class ORES_TRADING_CORE_EXPORT script_publish_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.script_publish_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit script_publish_service(const ores::database::context& ctx);

    messaging::publish_script_response publish(const messaging::publish_script_request& request);

private:
    ores::database::context ctx_;
};

}

#endif
