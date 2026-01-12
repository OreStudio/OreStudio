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
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.cli/config/options.hpp"
#include "ores.cli/config/import_options.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.cli/config/delete_options.hpp"
#include "ores.cli/config/add_options.hpp"
#include "ores.cli/config/add_currency_options.hpp"
#include "ores.cli/config/add_account_options.hpp"
#include "ores.cli/config/add_feature_flag_options.hpp"
#include "ores.cli/config/add_login_info_options.hpp"
#include "ores.cli/config/add_role_options.hpp"
#include "ores.cli/config/add_permission_options.hpp"
#include "ores.cli/config/add_country_options.hpp"
#include "ores.cli/config/add_change_reason_options.hpp"
#include "ores.cli/config/add_change_reason_category_options.hpp"

namespace ores::cli::app {

/**
 * @brief Entry point for the ores command line application.
 */
class application final {
private:
    inline static std::string_view logger_name =
        "ores.cli.application";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit application(std::ostream& output_stream,
        const std::optional<database::database_options>& db_opts);
    application(const application&) = delete;
    application& operator=(const application&) = delete;

private:
    static database::context
    make_context(const std::optional<database::database_options>& db_opts);
    void import_currencies(const std::vector<std::filesystem::path> files) const;
    void import_data(const std::optional<config::import_options>& ocfg) const;

    void export_currencies(const config::export_options& cfg) const;
    void export_accounts(const config::export_options& cfg) const;
    void export_feature_flags(const config::export_options& cfg) const;
    void export_login_info(const config::export_options& cfg) const;
    void export_roles(const config::export_options& cfg) const;
    void export_permissions(const config::export_options& cfg) const;
    void export_countries(const config::export_options& cfg) const;
    void export_change_reasons(const config::export_options& cfg) const;
    void export_change_reason_categories(const config::export_options& cfg) const;
    void export_data(const std::optional<config::export_options>& ocfg) const;

    void delete_currency(const config::delete_options& cfg) const;
    void delete_account(const config::delete_options& cfg) const;
    void delete_feature_flag(const config::delete_options& cfg) const;
    void delete_login_info(const config::delete_options& cfg) const;
    void delete_role(const config::delete_options& cfg) const;
    void delete_permission(const config::delete_options& cfg) const;
    void delete_country(const config::delete_options& cfg) const;
    void delete_change_reason(const config::delete_options& cfg) const;
    void delete_change_reason_category(const config::delete_options& cfg) const;
    void delete_data(const std::optional<config::delete_options>& ocfg) const;

    void add_currency(const config::add_currency_options& cfg) const;
    void add_account(const config::add_account_options& cfg) const;
    void add_feature_flag(const config::add_feature_flag_options& cfg) const;
    void add_login_info(const config::add_login_info_options& cfg) const;
    void add_role(const config::add_role_options& cfg) const;
    void add_permission(const config::add_permission_options& cfg) const;
    void add_country(const config::add_country_options& cfg) const;
    void add_change_reason(const config::add_change_reason_options& cfg) const;
    void add_change_reason_category(const config::add_change_reason_category_options& cfg) const;
    void add_data(const std::optional<config::add_options>& ocfg) const;

public:
    /**
     * @brief Executes the application.
     *
     * @param cfg Application configuration.
     */
    void run(const config::options& cfg) const;

private:
    database::context context_;
    std::ostream& output_stream_;
};

}

#endif
