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
#ifndef ORES_ORE_SERVICE_MESSAGING_REPORT_PACKAGE_HANDLER_HPP
#define ORES_ORE_SERVICE_MESSAGING_REPORT_PACKAGE_HANDLER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::ore::service::messaging {

/**
 * @brief Workflow step handler for prepare_ore_package.
 *
 * Downloads the trade and market data MsgPack blobs produced by the
 * gather steps, repackages them as a .tar.gz bundle in object storage,
 * and returns the tarball storage key.
 *
 * This is a stub implementation: the tarball contains the raw MsgPack
 * blobs rather than a fully formatted ORE input package.  ORE XML
 * generation will be added in a subsequent sprint.
 */
class report_package_handler {
private:
    inline static std::string_view logger_name =
        "ores.ore.service.messaging.report_package_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    report_package_handler(ores::nats::service::client& nats,
        std::string http_base_url);

    void prepare_package(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    std::string http_base_url_;
};

}

#endif
