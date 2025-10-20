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
#ifndef ORES_CLI_APP_APPLICATION_HPP
#define ORES_CLI_APP_APPLICATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <vector>
#include <filesystem>
#include <boost/asio/awaitable.hpp>
#include "ores.cli/config/options.hpp"
#include "ores.cli/config/import_options.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.comms/client.hpp"
#include "ores.risk/repository/context.hpp"

namespace ores::cli::app {

/**
 * @brief Entry point for the ores command line application.
 */
class application final {
public:
    application();
    application(const application&) = delete;
    application& operator=(const application&) = delete;

private:
    static risk::repository::context make_context();
    void import_currencies(const std::vector<std::filesystem::path> files) const;
    void import_data(const std::optional<config::import_options>& ocfg) const;

    void export_currencies(const config::export_options& cfg) const;
    void export_data(const std::optional<config::export_options>& ocfg) const;

    boost::asio::awaitable<void> run_client() const;

public:
    /**
     * @brief Executes the application.
     *
     * @param cfg Application configuration.
     */
    boost::asio::awaitable<void> run(const config::options& cfg) const;

private:
    risk::repository::context context_;
};

}

#endif
