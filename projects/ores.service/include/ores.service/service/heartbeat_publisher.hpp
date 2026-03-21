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
#ifndef ORES_SERVICE_SERVICE_HEARTBEAT_PUBLISHER_HPP
#define ORES_SERVICE_SERVICE_HEARTBEAT_PUBLISHER_HPP

#include <string>
#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.telemetry/messaging/service_samples_protocol.hpp"

namespace ores::service::service {

/**
 * @brief Background coroutine that publishes a heartbeat on a timer.
 *
 * Embed one per domain service by co_spawning run() from the on_started
 * callback in ores::service::service::run().  The heartbeat is received
 * by the telemetry service which persists it to
 * ores_telemetry_service_samples_tbl, enabling the service dashboard to
 * show live RAG status for every running service instance.
 *
 * A unique instance_id UUID is generated at construction time so that
 * multiple instances of the same service can be tracked independently.
 */
class heartbeat_publisher {
private:
    inline static std::string_view logger_name =
        "ores.service.service.heartbeat_publisher";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    heartbeat_publisher(std::string service_name,
                        std::string version,
                        ores::nats::service::client& nats,
                        std::uint32_t interval_seconds = 15)
        : service_name_(std::move(service_name))
        , version_(std::move(version))
        , nats_(nats)
        , interval_seconds_(interval_seconds) {
        // Generate a stable per-process instance UUID.
        ores::utility::uuid::uuid_v7_generator gen;
        instance_id_ = boost::uuids::to_string(gen());
    }

    /**
     * @brief Runs the heartbeat loop as an awaitable coroutine.
     *
     * Publishes one heartbeat immediately on start, then repeats every
     * interval_seconds.  Exits cleanly when the io_context shuts down.
     */
    boost::asio::awaitable<void> run() {
        BOOST_LOG_SEV(lg(), info)
            << "Heartbeat publisher started for '" << service_name_
            << "' instance=" << instance_id_
            << " interval=" << interval_seconds_ << "s";

        auto executor = co_await boost::asio::this_coro::executor;
        boost::asio::steady_timer timer(executor);

        try {
            for (;;) {
                publish_once();
                timer.expires_after(
                    std::chrono::seconds(interval_seconds_));
                co_await timer.async_wait(boost::asio::use_awaitable);
            }
        } catch (const boost::system::system_error& e) {
            if (e.code() != boost::asio::error::operation_aborted) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Heartbeat timer error: " << e.what();
            }
        }

        BOOST_LOG_SEV(lg(), info)
            << "Heartbeat publisher stopped for '" << service_name_ << "'";
    }

private:
    void publish_once() {
        try {
            telemetry::messaging::service_heartbeat_message hb;
            hb.service_name = service_name_;
            hb.instance_id = instance_id_;
            hb.version = version_;

            const auto json = rfl::json::write(hb);
            std::vector<std::byte> data(
                reinterpret_cast<const std::byte*>(json.data()),
                reinterpret_cast<const std::byte*>(json.data() + json.size()));
            nats_.publish(
                telemetry::messaging::service_heartbeat_message::nats_subject,
                std::move(data));
            BOOST_LOG_SEV(lg(), trace)
                << "Heartbeat published: " << service_name_;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to publish heartbeat: " << e.what();
        }
    }

    std::string service_name_;
    std::string instance_id_;
    std::string version_;
    ores::nats::service::client& nats_;
    std::uint32_t interval_seconds_;
};

} // namespace ores::service::service

#endif
