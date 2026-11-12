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
#ifndef ORES_HTTP_ROUTES_ASSETS_IMAGE_SCREEN_ROUTES_HPP
#define ORES_HTTP_ROUTES_ASSETS_IMAGE_SCREEN_ROUTES_HPP

#include "ores.assets.api/domain/image.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/business_centre.hpp"
#include "ores.refdata.api/domain/calendar.hpp"
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::assets {

/**
 * @brief The image map: which image every record that shows a flag uses, by
 * its code, and the placeholder drawn for a code with none.
 *
 * A calendar uses its own image when it has one, else its country's; a
 * business centre uses its country's. It holds identifiers only: a screen
 * fetches each image once from the image's own address.
 */
struct image_map final {
    std::map<std::string, std::string> currencies;
    std::map<std::string, std::string> countries;
    std::map<std::string, std::string> calendars;
    std::map<std::string, std::string> businessCentres;
    std::optional<std::string> noFlag;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<image_map>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"currencies", &image_map::currencies},
                                              field{"countries", &image_map::countries},
                                              field{"calendars", &image_map::calendars},
                                              field{"businessCentres", &image_map::businessCentres},
                                              field{"noFlag", &image_map::noFlag}};
}

/**
 * @brief The image screens' reads and upload, at the addresses and in the
 * shapes ores.web reads.
 *
 * A page of the tenant's images, one image's bytes, the policy an upload must
 * meet, an upload, and the image map view. Each forwards the caller's own
 * token, so the owning service's permission check governs.
 */
class image_screen_routes final {
public:
    /// The image code of the placeholder flag.
    static constexpr std::string_view no_flag_code = "xx";

    /**
     * @brief The image map from the four reference data lists and the images
     * a search for the placeholder's code found.
     */
    static image_map
    image_map_of(const std::vector<ores::refdata::domain::currency>& currencies,
                 const std::vector<ores::refdata::domain::country>& countries,
                 const std::vector<ores::refdata::domain::calendar>& calendars,
                 const std::vector<ores::refdata::domain::business_centre>& centres,
                 const std::vector<ores::assets::domain::image>& placeholders);

    /**
     * @brief The most an upload body may hold, as the BFF allowed.
     */
    static constexpr std::uint64_t upload_body_bytes = 4 * 1024 * 1024;

    /**
     * @brief A page of images a query names.
     */
    struct image_page final {
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;
        std::string search;
    };

    /**
     * @brief An upload: the media type and the image bytes in base64.
     */
    struct upload final {
        std::string mime_type;
        std::string data;
    };

    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The page a query names, or nothing when it is not one: an offset, a
     * limit of 1 to 500 and a search of at most 256 characters once trimmed.
     */
    [[nodiscard]] static std::optional<image_page>
    read_page(const ores::http::domain::http_request& req);

    /**
     * @brief An upload a body states, or nothing when it is not one: a media
     * type of 1 to 100 characters and some data.
     */
    [[nodiscard]] static std::optional<upload> read_upload(const std::string& body);
};

}

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::assets::image_map>
    : shaped_wire_traits<ores::http::routes::assets::image_map> {};

#endif
