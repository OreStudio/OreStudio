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
#ifndef ORES_HTTP_API_NET_IMAGE_ANSWER_HPP
#define ORES_HTTP_API_NET_IMAGE_ANSWER_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/export.hpp"
#include <string>

namespace ores::http::net {

/**
 * @brief An image as the browser shows it: its bytes, kept privately for five
 * minutes and never run as a document.
 *
 * A stored media type is served only when it names an image; an empty one is
 * SVG, the store's default, and any other is served as octet-stream, so an image
 * whose stored type names a document is never read as one.
 */
[[nodiscard]] ORES_HTTP_API_EXPORT domain::http_response image_answer(const std::string& mime_type,
                                                                      std::string bytes);

}

#endif
