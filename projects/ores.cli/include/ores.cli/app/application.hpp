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

#include <vector>
#include <ostream>
#include <optional>
#include <filesystem>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"
#include "ores.utility/database/database_options.hpp"
#include "ores.cli/config/options.hpp"
#include "ores.cli/config/import_options.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.cli/config/delete_options.hpp"

namespace ores::cli::app {

/**
 * @brief Entry point for the ores command line application.
 */
class application final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.cli.application");
        return instance;
    }

public:
    explicit application(std::ostream& output_stream,
        const std::optional<utility::database::database_options>& db_opts);
    application(const application&) = delete;
    application& operator=(const application&) = delete;

private:
    static utility::repository::context
    make_context(const std::optional<utility::database::database_options>& db_opts);
    void import_currencies(const std::vector<std::filesystem::path> files) const;
    void import_data(const std::optional<config::import_options>& ocfg) const;

    void export_currencies(const config::export_options& cfg) const;
    void export_data(const std::optional<config::export_options>& ocfg) const;

    void delete_account(const config::delete_options& cfg) const;
    void delete_data(const std::optional<config::delete_options>& ocfg) const;

public:
    /**
     * @brief Executes the application.
     *
     * @param cfg Application configuration.
     */
    void run(const config::options& cfg) const;

private:
    utility::repository::context context_;
    std::ostream& output_stream_;
};

}

#endif
