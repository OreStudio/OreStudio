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
#ifndef ORES_DQ_API_MESSAGING_PUBLISH_PARAMS_HPP
#define ORES_DQ_API_MESSAGING_PUBLISH_PARAMS_HPP

#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief Per-party parameters for a publication that names its parties.
 */
struct lei_parties_params {
    std::string root_lei;
};

/**
 * @brief The members of lei_parties_params, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<lei_parties_params>) {
    using ::ores::utility::codec::field;
    return std::tuple{field{"root_lei", &lei_parties_params::root_lei}};
}

static_assert(::ores::utility::codec::names_every_member<lei_parties_params>(),
              "lei_parties_params gained a member its wire_fields does not name.");

/**
 * @brief The per-dataset parameters a bundle publication carries.
 *
 * Deliberately narrower than the fields a bundle itself holds: this is what a
 * caller may vary per publication, not what the bundle declares.
 */
struct publish_bundle_params {
    std::vector<std::string> opted_in_datasets;
    std::optional<lei_parties_params> lei_parties;
    std::optional<std::string> party_id;
    std::optional<std::string> tiers;
    std::optional<std::string> jurisdiction;
};

/**
 * @brief The members of publish_bundle_params, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_bundle_params>) {
    using ::ores::utility::codec::field;
    return std::tuple{field{"opted_in_datasets", &publish_bundle_params::opted_in_datasets},
                      field{"lei_parties", &publish_bundle_params::lei_parties},
                      field{"party_id", &publish_bundle_params::party_id},
                      field{"tiers", &publish_bundle_params::tiers},
                      field{"jurisdiction", &publish_bundle_params::jurisdiction}};
}

static_assert(::ores::utility::codec::names_every_member<publish_bundle_params>(),
              "publish_bundle_params gained a member its wire_fields does not name.");

/**
 * @brief Renders the parameters as the JSON the publish path carries.
 */
inline std::string build_params_json(const publish_bundle_params& params) {
    return ores::utility::codec::json::write(params);
}

/**
 * @brief The per-dataset parameters a direct dataset publication carries.
 *
 * Deliberately narrower than publish_bundle_params, which carries bundle-only
 * fields (opted_in_datasets, lei_parties) that a direct publication does not
 * use.
 */
struct dataset_publish_params {
    std::optional<std::string> party_id;
};

/**
 * @brief The members of dataset_publish_params, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_publish_params>) {
    using ::ores::utility::codec::field;
    return std::tuple{field{"party_id", &dataset_publish_params::party_id}};
}

static_assert(::ores::utility::codec::names_every_member<dataset_publish_params>(),
              "dataset_publish_params gained a member its wire_fields does not name.");

/**
 * @brief Renders the parameters as the JSON the publish path carries.
 */
inline std::string build_params_json(const dataset_publish_params& params) {
    return ores::utility::codec::json::write(params);
}

}

#endif
