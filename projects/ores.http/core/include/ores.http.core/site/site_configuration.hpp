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
#ifndef ORES_HTTP_CORE_SITE_SITE_CONFIGURATION_HPP
#define ORES_HTTP_CORE_SITE_SITE_CONFIGURATION_HPP

#include "ores.http.core/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

/**
 * @brief What a deployment tells the browser about itself.
 *
 * The site configuration is the file the BFF reads, =environments.json=: the
 * environments a deployment may serve, the certificates they share, and whether
 * the developer surface is offered. The server reads the same file, chosen the
 * same way, so moving the browser from the BFF to the server changes nothing a
 * deployment states.
 */
namespace ores::http::site {

/**
 * @brief Certificate paths an environment overrides, each absent when it does not.
 */
struct environment_tls final {
    std::optional<std::string> ca;
    std::optional<std::string> cert;
    std::optional<std::string> key;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<environment_tls>) {
    using ores::utility::codec::field;
    return std::tuple{field{"ca", &environment_tls::ca},
                      field{"cert", &environment_tls::cert},
                      field{"key", &environment_tls::key}};
}

/**
 * @brief One environment a deployment may serve.
 */
struct environment final {
    std::string id;
    std::string displayName;
    std::string description;
    std::string host;
    std::int64_t port = 0;
    std::string subjectPrefix;
    std::string httpBaseUrl;
    environment_tls tls;
    bool nonProduction = true;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<environment>) {
    using ores::utility::codec::field;
    return std::tuple{field{"id", &environment::id},
                      field{"displayName", &environment::displayName},
                      field{"description", &environment::description},
                      field{"host", &environment::host},
                      field{"port", &environment::port},
                      field{"subjectPrefix", &environment::subjectPrefix},
                      field{"httpBaseUrl", &environment::httpBaseUrl},
                      field{"tls", &environment::tls},
                      field{"nonProduction", &environment::nonProduction}};
}

/**
 * @brief The certificate paths every environment shares.
 */
struct shared_tls final {
    std::string ca;
    std::string cert;
    std::string key;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<shared_tls>) {
    using ores::utility::codec::field;
    return std::tuple{field{"ca", &shared_tls::ca},
                      field{"cert", &shared_tls::cert},
                      field{"key", &shared_tls::key}};
}

/**
 * @brief A test identity the developer surface offers.
 */
struct developer_account final {
    std::string username;
    std::string label;
    std::string description;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<developer_account>) {
    using ores::utility::codec::field;
    return std::tuple{field{"username", &developer_account::username},
                      field{"label", &developer_account::label},
                      field{"description", &developer_account::description}};
}

/**
 * @brief The site configuration file, as it is written.
 */
struct site_configuration final {
    std::string active;
    std::vector<environment> environments;
    shared_tls tls;
    bool developerTools = false;
    std::vector<developer_account> developerAccounts;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<site_configuration>) {
    using ores::utility::codec::field;
    return std::tuple{field{"active", &site_configuration::active},
                      field{"environments", &site_configuration::environments},
                      field{"tls", &site_configuration::tls},
                      field{"developerTools", &site_configuration::developerTools},
                      field{"developerAccounts", &site_configuration::developerAccounts}};
}

/**
 * @brief A site configuration resolved for this deployment.
 */
struct loaded_site final {
    /// The file, with the environment it serves named active and the developer
    /// surface as the deployment overrides it.
    site_configuration configuration;
    /// The environment this deployment serves.
    environment served;
    /// Where the file was read from.
    std::string source;
};

/**
 * @brief A site configuration that cannot be read or used.
 */
class ORES_HTTP_CORE_EXPORT site_configuration_error final : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * @brief Reads a process variable, or nothing when it is unset.
 */
using variable_lookup = std::function<std::optional<std::string>(std::string_view)>;

/**
 * @brief The variable that names the file, as the BFF reads it.
 */
inline constexpr std::string_view site_config_variable = "ORES_WEB_SITE_CONFIG";

/**
 * @brief Load the site configuration the way the BFF does.
 *
 * The file is @p path, else the one =ORES_WEB_SITE_CONFIG= names. The
 * environment is @p environment_id, else =ORES_WEB_ENV=, else the checkout's
 * =ORES_ENV_NAME= with its dashes read as underscores, else the file's active
 * environment, else its first. =ORES_WEB_DEVELOPER_TOOLS= overrides the file's
 * developer surface. Throws site_configuration_error naming what is wrong.
 */
[[nodiscard]] ORES_HTTP_CORE_EXPORT loaded_site
load_site(const std::string& path, const std::string& environment_id, const variable_lookup& lookup);

/**
 * @brief Read and check a site configuration from its text.
 */
[[nodiscard]] ORES_HTTP_CORE_EXPORT site_configuration parse_site(std::string_view text,
                                                                  std::string_view source);

}

#endif
