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
#ifndef ORES_COMMS_ANALYSER_APP_APPLICATION_HPP
#define ORES_COMMS_ANALYSER_APP_APPLICATION_HPP

#include "ores.utility/log/make_logger.hpp"
#include "ores.comms.analyser/config/options.hpp"
#include "ores.comms.analyser/domain/session_reader.hpp"

namespace ores::comms::analyser::app {

/**
 * @brief Main application for the session analyser.
 */
class application final {
private:
    inline static std::string_view logger_name = "ores.comms.analyser.app.application";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct the application with the given options.
     */
    explicit application(config::options opts);

    /**
     * @brief Run the application.
     *
     * @return Exit code (0 for success)
     */
    int run();

private:
    /**
     * @brief Execute the read command.
     */
    int read_session();

    /**
     * @brief Execute the info command.
     */
    int show_info();

    /**
     * @brief Print session header information.
     */
    void print_header(const domain::session_metadata& metadata);

    /**
     * @brief Print frame list in tshark-like format.
     */
    void print_frames(const domain::session_data& data);

    /**
     * @brief Format a timestamp offset as a string.
     */
    static std::string format_timestamp(std::int64_t offset_us);

    /**
     * @brief Get a short description for a message type.
     */
    static std::string get_message_info(const comms::messaging::frame& f);

    config::options opts_;
};

}

#endif
