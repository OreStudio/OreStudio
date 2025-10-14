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
#include "ores.comms/connection.hpp"
#include <boost/asio/read.hpp>
#include <boost/asio/write.hpp>

namespace ores::comms {

connection::connection(ssl_socket socket) : socket_(std::move(socket)) {}

cobalt::promise<void> connection::ssl_handshake_server() {
    co_await socket_.async_handshake(ssl::stream_base::server, cobalt::use_op);
}

cobalt::promise<void> connection::ssl_handshake_client() {
    co_await socket_.async_handshake(ssl::stream_base::client, cobalt::use_op);
}

cobalt::promise<std::expected<protocol::frame, protocol::error_code>> connection::read_frame() {
    try {
        // Read frame header first (32 bytes)
        std::array<uint8_t, protocol::frame_header::size> header_buffer;
        co_await boost::asio::async_read(
            socket_,
            boost::asio::buffer(header_buffer),
            cobalt::use_op);

        // Deserialize header to get payload size
        std::span<const uint8_t> header_span(header_buffer.data(), header_buffer.size());
        auto header_result = protocol::frame::deserialize(header_span);
        if (!header_result) {
            co_return std::unexpected(header_result.error());
        }

        // Get payload size from header
        const auto& header = header_result->header();
        uint32_t payload_size = header.payload_size;

        // Read payload if present
        std::vector<uint8_t> payload_buffer;
        if (payload_size > 0) {
            payload_buffer.resize(payload_size);
            co_await boost::asio::async_read(
                socket_,
                boost::asio::buffer(payload_buffer),
                cobalt::use_op);
        }

        // Combine header and payload for full frame deserialization
        std::vector<uint8_t> full_frame_buffer;
        full_frame_buffer.reserve(protocol::frame_header::size + payload_size);
        full_frame_buffer.insert(full_frame_buffer.end(), header_buffer.begin(), header_buffer.end());
        full_frame_buffer.insert(full_frame_buffer.end(), payload_buffer.begin(), payload_buffer.end());

        // Deserialize and validate complete frame
        auto frame_result = protocol::frame::deserialize(full_frame_buffer);
        co_return frame_result;

    } catch (const std::exception& e) {
        co_return std::unexpected(protocol::error_code::invalid_message_type);
    }
}

cobalt::promise<void> connection::write_frame(const protocol::frame& frame) {
    auto data = frame.serialize();
    co_await boost::asio::async_write(
        socket_,
        boost::asio::buffer(data),
        cobalt::use_op);
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
    } catch (const std::exception&) {
        return "unknown";
    }
}

}
