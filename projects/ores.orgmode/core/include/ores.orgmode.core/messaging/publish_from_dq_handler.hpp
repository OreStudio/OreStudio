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
#ifndef ORES_ORGMODE_CORE_MESSAGING_PUBLISH_FROM_DQ_HANDLER_HPP
#define ORES_ORGMODE_CORE_MESSAGING_PUBLISH_FROM_DQ_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/delivery.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.orgmode.core/export.hpp"
#include <string_view>

namespace ores::orgmode::messaging {

/**
 * @brief Handles the orgmode.v1.ops.publish_knowledge_pages_from_dq workflow step.
 *
 * Calls ores_orgmode_publish_knowledge_pages_from_dq_fn, a SECURITY DEFINER
 * function that reads the DQ artefact table and writes the knowledge pages.
 */
class ORES_ORGMODE_CORE_EXPORT publish_from_dq_handler {
private:
    inline static std::string_view logger_name = "ores.orgmode.messaging.publish_from_dq_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    publish_from_dq_handler(ores::nats::service::client& nats, const ores::database::context& ctx);

    void handle(ores::nats::delivery delivery);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
