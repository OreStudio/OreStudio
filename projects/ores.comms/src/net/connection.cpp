/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.comms/net/connection.hpp"

#include <boost/asio/read.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/asio/bind_cancellation_slot.hpp>

namespace ores::comms::net {

using namespace ores::logging;

connection::connection(ssl_socket socket) : socket_(std::move(socket)) {}

boost::asio::awaitable<void> connection::ssl_handshake_server(
    boost::asio::cancellation_slot cancel_slot) {
    co_await socket_.async_handshake(boost::asio::ssl::stream_base::server,
        boost::asio::bind_cancellation_slot(cancel_slot, boost::asio::use_awaitable));
}

boost::asio::awaitable<void> connection::ssl_handshake_client(
    boost::asio::cancellation_slot cancel_slot) {
    co_await socket_.async_handshake(boost::asio::ssl::stream_base::client,
        boost::asio::bind_cancellation_slot(cancel_slot, boost::asio::use_awaitable));
}

boost::asio::awaitable<read_frame_result>
connection::read_frame(bool skip_version_check, boost::asio::cancellation_slot cancel_slot) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Waiting to read the next frame"
                                 << " (skip_version_check=" << skip_version_check << ")"
                                 << " cancel_slot.is_connected=" << cancel_slot.is_connected();

        // Read the fixed 32-byte header first
        std::vector<std::byte> buffer(messaging::frame_header::size);
        co_await boost::asio::async_read(
            socket_,
            boost::asio::buffer(buffer),
            boost::asio::bind_cancellation_slot(cancel_slot, boost::asio::use_awaitable));

        BOOST_LOG_SEV(lg(), debug) << "Read header of size: "
                                 << messaging::frame_header::size;

        // Deserialize and validate the header.
        // validates magic, version, type, reserved fields, payload size.
        auto header_result = messaging::frame::deserialize_header(
            std::span<const std::byte>(buffer), skip_version_check);
        if (!header_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize header, error: "
                                     << static_cast<int>(header_result.error());
            co_return read_frame_result{std::unexpected(header_result.error()), std::nullopt};
        }

        const auto& header = *header_result;
        BOOST_LOG_SEV(lg(), debug) << "Header payload size: "
                                 << header.payload_size;

        // Read payload if any.
        if (header.payload_size > 0) {
            buffer.resize(messaging::frame_header::size + header.payload_size);
            co_await boost::asio::async_read(socket_,
                boost::asio::buffer(buffer.data() + messaging::frame_header::size,
                    header.payload_size),
                boost::asio::bind_cancellation_slot(cancel_slot, boost::asio::use_awaitable));

            BOOST_LOG_SEV(lg(), debug) << "Read payload of size: " << header.payload_size;
        }

        // Deserialize the complete frame (validates CRC)
        auto frame_result = messaging::frame::deserialize(header,
            std::span<const std::byte>(buffer));
        if (!frame_result) {
            const auto ec = frame_result.error();
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize frame, error: "
                                     << static_cast<int>(ec)
                                     << " type=" << header.type
                                     << " correlation_id=" << header.correlation_id
                                     << " payload_size=" << header.payload_size;

            // On CRC failure, return the correlation_id so caller can fail the pending request
            if (ec == ores::utility::serialization::error_code::crc_validation_failed) {
                co_return read_frame_result{std::unexpected(ec), header.correlation_id};
            }
            co_return read_frame_result{std::unexpected(ec), std::nullopt};
        }

        BOOST_LOG_SEV(lg(), debug) << "Successfully deserialized frame, type: "
                                 << frame_result->header().type
                                 << " total size: " << buffer.size();

        // Track bytes received
        bytes_received_.fetch_add(buffer.size(), std::memory_order_relaxed);

        co_return read_frame_result{std::move(frame_result), std::nullopt};

    } catch (const boost::system::system_error& e) {
        // Operation canceled is expected during shutdown - log at debug level
        if (e.code() == boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), debug) << "Read cancelled due to shutdown.";
        } else {
            BOOST_LOG_SEV(lg(), error) << "Network error in read_frame: "
                                       << e.what();
        }
        co_return read_frame_result{
            std::unexpected(ores::utility::serialization::error_code::network_error),
            std::nullopt};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unexpected error in read_frame: "
                                 << e.what();
        co_return read_frame_result{
            std::unexpected(ores::utility::serialization::error_code::invalid_message_type),
            std::nullopt};
    }
}

boost::asio::awaitable<void>
connection::write_frame(const messaging::frame& frame,
    boost::asio::cancellation_slot cancel_slot) {
    auto data = frame.serialize();
    BOOST_LOG_SEV(lg(), debug) << "Writing frame of size " << data.size()
                             << " type: " << frame.header().type
                             << " sequence: " << frame.header().sequence;
    co_await boost::asio::async_write(
        socket_,
        boost::asio::buffer(data),
        boost::asio::bind_cancellation_slot(cancel_slot, boost::asio::use_awaitable));

    // Track bytes sent
    bytes_sent_.fetch_add(data.size(), std::memory_order_relaxed);

    BOOST_LOG_SEV(lg(), debug) << "Successfully wrote frame";
}

bool connection::is_open() const {
    return socket_.lowest_layer().is_open();
}

void connection::close() {
    if (is_open()) {
        boost::system::error_code ec;
        socket_.lowest_layer().close(ec);
    }
}

std::string connection::remote_address() const {
    try {
        auto endpoint = socket_.lowest_layer().remote_endpoint();
        return endpoint.address().to_string() + ":" + std::to_string(endpoint.port());
    } catch (const std::exception& e) {
        std::string msg(std::format("Error: {}", e.what()));
        BOOST_LOG_SEV(lg(), error) << msg;
        return msg;
    }
}

std::uint64_t connection::bytes_sent() const {
    return bytes_sent_.load(std::memory_order_relaxed);
}

std::uint64_t connection::bytes_received() const {
    return bytes_received_.load(std::memory_order_relaxed);
}

void connection::reset_byte_counters() {
    bytes_sent_.store(0, std::memory_order_relaxed);
    bytes_received_.store(0, std::memory_order_relaxed);
}

}
