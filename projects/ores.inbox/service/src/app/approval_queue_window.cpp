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
#include "ores.inbox.service/app/approval_queue_window.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include <stdexcept>
#include <string>

namespace ores::inbox::service::app {

namespace {

/**
 * @brief The setting the window comes from.
 */
constexpr std::string_view window_setting_name = "inbox.approval_queue.answered_window_seconds";

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.inbox.service.app.approval_queue_window");
    return instance;
}

}

std::chrono::seconds answered_window_seconds(ores::nats::service::nats_client& svc_nats) {
    using namespace ores::logging;
    using ores::utility::domain::outcome;

    ores::variability::messaging::get_setting_request request;
    request.name = std::string(window_setting_name);

    try {
        const auto& codec = ores::nats::default_wire_codec();
        const auto reply =
            svc_nats.authenticated_request(request.nats_subject, codec.encode(request));
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end())
            throw std::runtime_error("refused: " + it->second);

        auto answer = codec.decode<ores::variability::messaging::get_setting_response>(reply.data);
        if (!answer)
            throw std::runtime_error("the answer could not be read");
        if (answer->result.outcome != outcome::ok)
            throw std::runtime_error(answer->result.message.empty() ? "no such setting"
                                                                    : answer->result.message);

        const auto seconds = std::stoll(answer->value);
        if (seconds <= 0)
            throw std::runtime_error("the setting is " + answer->value +
                                     ", which is not a length of time");
        BOOST_LOG_SEV(lg(), info) << "The answered queue tail reaches back " << seconds
                                  << " seconds, from " << window_setting_name << ".";
        return std::chrono::seconds(seconds);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "Could not read " << window_setting_name << " (" << e.what()
            << "), so the queue will answer no answered tail. The open queue is unaffected.";
        return std::chrono::seconds::zero();
    }
}

}
