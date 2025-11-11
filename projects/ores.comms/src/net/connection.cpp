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

namespace ores::comms {

using namespace ores::utility::log;

connection::connection(ssl_socket socket) : socket_(std::move(socket)) {}

boost::asio::awaitable<void> connection::ssl_handshake_server() {
    co_await socket_.async_handshake(boost::asio::ssl::stream_base::server,
        boost::asio::use_awaitable);
}

boost::asio::awaitable<void> connection::ssl_handshake_client() {
    co_await socket_.async_handshake(boost::asio::ssl::stream_base::client,
        boost::asio::use_awaitable);
}

boost::asio::awaitable<std::expected<protocol::frame, protocol::error_code>>
connection::read_frame() {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Waiting to read the next frame.";

        // Read the fixed 32-byte header first
        std::vector<std::uint8_t> buffer(protocol::frame_header::size);
        co_await boost::asio::async_read(
            socket_,
            boost::asio::buffer(buffer),
            boost::asio::use_awaitable);

        BOOST_LOG_SEV(lg(), debug) << "Read header of size: "
                                 << protocol::frame_header::size;

        // Deserialize and validate the header.
        // validates magic, version, type, reserved fields, payload size.
        auto header_result = protocol::frame::deserialize_header(
            std::span<const std::uint8_t>(buffer));
        if (!header_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize header, error: "
                                     << static_cast<int>(header_result.error());
            co_return std::unexpected(header_result.error());
        }

        const auto& header = *header_result;
        BOOST_LOG_SEV(lg(), debug) << "Header payload size: "
                                 << header.payload_size;

        // Read payload if any.
        if (header.payload_size > 0) {
            buffer.resize(protocol::frame_header::size + header.payload_size);
            co_await boost::asio::async_read(socket_,
                boost::asio::buffer(buffer.data() + protocol::frame_header::size,
                    header.payload_size),
                boost::asio::use_awaitable);

            BOOST_LOG_SEV(lg(), debug) << "Read payload of size: " << header.payload_size;
        }

        // Deserialize the complete frame (validates CRC)
        auto frame_result = protocol::frame::deserialize(header,
            std::span<const std::uint8_t>(buffer));
        if (!frame_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize frame, error: "
                                     << static_cast<int>(frame_result.error());
            co_return std::unexpected(frame_result.error());
        }

        BOOST_LOG_SEV(lg(), debug) << "Successfully deserialized frame, type: "
                                 << static_cast<int>(frame_result->header().type)
                                 << " total size: " << buffer.size();
        co_return frame_result;

    } catch (const boost::system::system_error& e) {
        BOOST_LOG_SEV(lg(), error) << "Network error in read_frame: "
                                 << e.what();
        co_return std::unexpected(protocol::error_code::network_error);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unexpected error in read_frame: "
                                 << e.what();
        co_return std::unexpected(protocol::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<void>
connection::write_frame(const protocol::frame& frame) {
    auto data = frame.serialize();
    BOOST_LOG_SEV(lg(), debug) << "Writing frame of size " << data.size()
                             << " type: " << static_cast<int>(frame.header().type)
                             << " sequence: " << frame.header().sequence;
    co_await boost::asio::async_write(
        socket_,
        boost::asio::buffer(data),
        boost::asio::use_awaitable);
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

}
