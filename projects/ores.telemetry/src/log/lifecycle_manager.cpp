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
#include "ores.telemetry/log/lifecycle_manager.hpp"

#include <string_view>
#include <boost/make_shared.hpp>
#include <boost/core/null_deleter.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/process/v2/pid.hpp>
#include <boost/log/core.hpp>
#include <boost/log/sinks.hpp>
#include <boost/log/common.hpp>
#include <boost/log/attributes.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/sources/logger.hpp>
#include <boost/log/support/date_time.hpp>
#include "ores.telemetry/log/logging_options_validator.hpp"

namespace ores::telemetry::log {

using namespace boost::log;

namespace {

/**
 * @brief Builds the log filename, optionally including the PID.
 */
std::string build_filename(const std::string& filename, bool include_pid) {
    if (!include_pid) {
        return filename;
    }

    const auto pid = std::to_string(boost::process::v2::current_pid());
    const auto dot_pos = filename.rfind('.');
    if (dot_pos != std::string::npos) {
        return filename.substr(0, dot_pos) + "." + pid + filename.substr(dot_pos);
    }
    return filename + "." + pid;
}

}

boost::shared_ptr<lifecycle_manager::file_sink_type>
lifecycle_manager::make_file_sink(std::filesystem::path path,
    const boost_severity severity, std::string tag) {

    const std::string extension(".log");
    if (path.extension() != extension)
        path += extension;

    auto backend(boost::make_shared<sinks::text_file_backend>(
            keywords::file_name = path.string(),
            keywords::rotation_size = 300 * 1024 * 1024,
            keywords::time_based_rotation =
            sinks::file::rotation_at_time_point(12, 0, 0)));
    backend->auto_flush(true);

    auto sink = boost::make_shared<file_sink_type>(backend);
    const std::string severity_attr("Severity");

    if (!tag.empty()) {
        const std::string tag_attr("Tag");
        sink->set_filter(
            expressions::attr<boost_severity>(severity_attr) >= severity &&
            expressions::has_attr(tag_attr) &&
            expressions::attr<std::string>(tag_attr) == tag);
    } else {
        sink->set_filter(expressions::attr<boost_severity>(
                severity_attr) >= severity);
    }

    const std::string channel_attr("Channel");
    const std::string time_stamp_attr("TimeStamp");
    const std::string record_format("%1% [%2%] [%3%] %4%");
    const std::string time_stamp_format("%Y-%m-%d %H:%M:%S.%f");
    sink->set_formatter(expressions::format(record_format)
        % expressions::format_date_time<boost::posix_time::ptime>(
            time_stamp_attr, time_stamp_format)
        % expressions::attr<boost_severity>(severity_attr)
        % expressions::attr<std::string_view>(channel_attr)
        % expressions::smessage);
    return sink;
}

boost::shared_ptr<lifecycle_manager::console_sink_type> lifecycle_manager::
make_console_sink(const boost_severity severity, std::string tag) {
    boost::shared_ptr<std::ostream> os(&std::cout, boost::null_deleter());
    auto backend(boost::make_shared<sinks::text_ostream_backend>());
    backend->add_stream(os);

    using sink_type = sinks::synchronous_sink<sinks::text_ostream_backend>;
    auto sink(boost::make_shared<sink_type>(backend));
    const std::string severity_attr("Severity");

    if (!tag.empty()) {
        const std::string tag_attr("Tag");
        sink->set_filter(
            expressions::attr<boost_severity>(severity_attr) >= severity &&
            expressions::has_attr(tag_attr) &&
            expressions::attr<std::string>(tag_attr) == tag);
    } else {
        sink->set_filter(expressions::attr<boost_severity>(
                severity_attr) >= severity);
    }

    const std::string channel_attr("Channel");
    const std::string time_stamp_attr("TimeStamp");
    const std::string time_stamp_format("%Y-%m-%d %H:%M:%S.%f");
    const std::string record_format("%1% [%2%] [%3%] %4%");
    sink->set_formatter(expressions::format(record_format)
        % expressions::format_date_time<boost::posix_time::ptime>(
            time_stamp_attr, time_stamp_format)
        % expressions::attr<boost_severity>(severity_attr)
        % expressions::attr<std::string_view>(channel_attr)
        % expressions::smessage);

    std::cout << "Initialised logging." <<std::endl;
    return sink;
}

lifecycle_manager::lifecycle_manager(std::optional<logging_options> ocfg) {
    /*
     * If no configuration is supplied, logging is to be disabled.
     */
    auto& core(*boost::log::core::get());
    if (!ocfg) {
        core.set_logging_enabled(false);
        return;
    }

    /*
     * A configuration was supplied. Ensure it is valid.
     */
    const auto& cfg(*ocfg);
    logging_options_validator::validate(cfg);
    core.set_logging_enabled(true);

    /*
     * Use the configuration to setup the logging infrastructure for both
     * console and file, if enabled. We don't have to worry about making sure
     * that at least one is enabled - that is the validator's job.
     */
    const auto sl(to_boost_severity(cfg.severity));
    if (cfg.output_to_console) {
        console_sink_ = make_console_sink(sl, cfg.tag);
        boost::log::core::get()->add_sink(console_sink_);
    }

    if (!cfg.filename.empty()) {
        const auto filename = build_filename(cfg.filename, cfg.include_pid);
        const auto path(cfg.output_directory / filename);
        file_sink_ = make_file_sink(path, sl, cfg.tag);
        boost::log::core::get()->add_sink(file_sink_);
    }

    /*
     * Finally, add the timestamp attributes.
     */
    const std::string time_stamp_attr("TimeStamp");
    core.add_global_attribute(time_stamp_attr,
        boost::log::attributes::local_clock());
}

lifecycle_manager::~lifecycle_manager() {
    auto core(boost::log::core::get());

    // Stop and flush the telemetry sink first (it's async)
    if (telemetry_sink_) {
        telemetry_sink_->stop();
        telemetry_sink_->flush();
        core->remove_sink(telemetry_sink_);
    }

    if (file_sink_)
        core->remove_sink(file_sink_);

    if (console_sink_)
        core->remove_sink(console_sink_);
}

void lifecycle_manager::add_telemetry_sink(
    std::shared_ptr<domain::resource> resource,
    telemetry_sink_backend::log_record_handler handler) {

    auto backend = boost::make_shared<telemetry_sink_backend>(
        std::move(resource), std::move(handler));

    telemetry_sink_ = boost::make_shared<telemetry_sink_type>(backend);

    // The telemetry sink receives all log records (no filtering by severity)
    // Filtering can be done in the handler if needed
    boost::log::core::get()->add_sink(telemetry_sink_);
}

}
