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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/xml/document_kind.hpp"
#include <cctype>
#include <fstream>
#include <string>

namespace ores::ore::xml {

namespace {

/// How much of a document is read to find its root element.
constexpr std::size_t kPeek = 4096;

std::string read_header(const std::filesystem::path& file) {
    std::ifstream ifs(file, std::ios::binary);
    if (!ifs)
        return {};
    std::string buf(kPeek, '\0');
    ifs.read(buf.data(), static_cast<std::streamsize>(kPeek));
    buf.resize(static_cast<std::size_t>(ifs.gcount()));
    return buf;
}

/**
 * @brief Reads the name of a document's root element.
 *
 * Leading comments, processing instructions and doctype declarations are
 * skipped, so the first start tag is the root however the file is
 * decorated.
 */
std::string read_root_element(const std::filesystem::path& file) {
    const auto header = read_header(file);
    auto at = header.find('<');
    while (at != std::string::npos) {
        if (header.compare(at, 4, "<!--") == 0) {
            const auto end = header.find("-->", at + 4);
            if (end == std::string::npos)
                return {};
            at = header.find('<', end + 3);
            continue;
        }
        if (header.compare(at, 2, "<?") == 0 || header.compare(at, 2, "<!") == 0) {
            const auto end = header.find('>', at + 2);
            if (end == std::string::npos)
                return {};
            at = header.find('<', end + 1);
            continue;
        }
        auto end = at + 1;
        while (end < header.size() && (std::isalpha(static_cast<unsigned char>(header[end])) ||
                                       header[end] == '_' || header[end] == ':'))
            ++end;
        return header.substr(at + 1, end - at - 1);
    }
    return {};
}

} // namespace

std::optional<document_kind> detect_document_kind(const std::filesystem::path& path) {
    const auto root = read_root_element(path);
    if (root == "Portfolio")
        return document_kind::portfolio;
    if (root == "CurrencyConfig")
        return document_kind::currency_config;
    if (root == "CalendarAdjustments")
        return document_kind::calendar_adjustments;
    if (root == "Conventions")
        return document_kind::conventions;
    return std::nullopt;
}

}
