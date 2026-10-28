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
#include "ores.ore.core/xml/run_package.hpp"
#include "ores.ore.core/domain/run_document_projection.hpp"
#include <algorithm>
#include <filesystem>
#include <format>
#include <stdexcept>

namespace ores::ore::xml {

namespace {

/**
 * @brief The run document out of a set of input files.
 */
domain::mapped_setup parse_run_document(const input_files& files) {
    const auto run = files.find(std::string(run_document_file));
    if (run == files.end())
        throw std::invalid_argument(
            std::format("An ORE input holds its run document as {}.", run_document_file));

    return domain::run_document_projection::map_setup(
        domain::run_document_projection::read(run->second, std::string(run_document_file)));
}

/**
 * @brief What a run document sets one setup parameter to, empty when none.
 */
std::string setup_parameter(const domain::mapped_setup& setup, std::string_view name) {
    if (name == "inputPath")
        return setup.settings.input_path;
    const auto file = std::find_if(setup.files.begin(), setup.files.end(), [name](const auto& f) {
        return f.parameter == name;
    });
    return file == setup.files.end() ? std::string{} : file->name;
}

/**
 * @brief Whether a path stays inside the package.
 *
 * The input path and the file names come from a stored run document, which a
 * tenant edits, and the result names where the package writes; a path that is
 * absolute or climbs out with .. would write outside the package.
 */
bool inside(const std::filesystem::path& p) {
    const auto normal = p.lexically_normal();
    if (p.is_absolute() || p.has_root_name() || normal.empty())
        return false;
    return std::ranges::none_of(normal, [](const auto& part) { return part == ".."; });
}

/**
 * @brief The run's data file in one slot, as the package path it goes to.
 *
 * Empty when the run document names no file there. A name that would leave the
 * package is refused rather than written.
 */
std::string
data_file_path(const std::string& input_path, std::string_view parameter, const std::string& name) {
    if (name.empty())
        return {};
    const std::filesystem::path path = std::filesystem::path(input_path) / name;
    if (!inside(std::filesystem::path(name)) || !inside(path))
        throw std::invalid_argument(std::format(
            "The run document's {} names {}, which would be written outside the package.",
            parameter,
            name));
    return path.lexically_normal().generic_string();
}

/**
 * @brief The input path the run document names, or its default.
 */
std::string input_path_of(const domain::mapped_setup& document) {
    std::string input_path = "Input";
    if (const auto declared = setup_parameter(document, "inputPath"); !declared.empty())
        input_path = declared;
    if (!inside(input_path))
        throw std::invalid_argument(
            std::format("The run document's input path {} leaves the package.", input_path));
    return input_path;
}

}

input_files archive_layout(const input_files& files) {
    const auto document = parse_run_document(files);
    const auto input_path = input_path_of(document);

    input_files layout;
    for (const auto& [name, content] : files) {
        const std::filesystem::path directory =
            name == run_document_file ? std::filesystem::path("Input") :
                                        std::filesystem::path(input_path);
        const auto path = directory / name;
        if (!inside(std::filesystem::path(name)) || !inside(path))
            throw std::invalid_argument(
                std::format("The input file {} would be written outside the package.", name));
        layout[path.lexically_normal().generic_string()] = content;
    }
    return layout;
}

run_data_files declared_data_files(const input_files& files) {
    const auto document = parse_run_document(files);
    const auto input_path = input_path_of(document);

    run_data_files places;
    places.market_data =
        data_file_path(input_path, "marketDataFile", setup_parameter(document, "marketDataFile"));
    places.fixings =
        data_file_path(input_path, "fixingDataFile", setup_parameter(document, "fixingDataFile"));
    places.portfolio =
        data_file_path(input_path, "portfolioFile", setup_parameter(document, "portfolioFile"));
    return places;
}

}
