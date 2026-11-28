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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SCHEDULER_API_MESSAGING_SCHEDULING_OPERATIONS_PROTOCOL_HPP
#define ORES_SCHEDULER_API_MESSAGING_SCHEDULING_OPERATIONS_PROTOCOL_HPP

#include "ores.scheduler.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::scheduler::messaging {

/**
 * @brief Asks for a page of job executions, newest first.
 *
 * Every job the caller's tenant owns is included, so the view answers
 * "what has the scheduler been running" without naming a job first.
 */
struct get_job_instances_request {
    using response_type = struct get_job_instances_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_instances.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;

    friend bool operator==(const get_job_instances_request&,
                           const get_job_instances_request&) = default;
};

/**
 * @brief One execution, with its job's display fields folded in.
 *
 * The definition identifier is the link to the job; the name and action type
 * come from the definition row so the caller does not look it up.
 */
struct job_instance_summary {
    /**
     * @brief The execution's storage key.
     */
    std::int64_t id = 0;
    /**
     * @brief The job that was executed.
     */
    std::string job_definition_id;
    /**
     * @brief The job's name, read from the definition.
     */
    std::string job_name;
    /**
     * @brief How the job was executed: =execute_sql= or =nats_publish=.
     */
    std::string action_type;
    /**
     * @brief The lifecycle state: =starting=, =succeeded= or =failed=.
     */
    std::string status;
    /**
     * @brief When the cron expression fired, in ISO-8601 UTC.
     */
    std::string triggered_at;
    /**
     * @brief When execution began, in ISO-8601 UTC.
     */
    std::string started_at;
    /**
     * @brief When execution ended, if it has ended.
     */
    std::optional<std::string> completed_at;
    /**
     * @brief How long execution took in milliseconds, if it has ended.
     */
    std::optional<std::int64_t> duration_ms;
    /**
     * @brief Why execution failed, when it failed.
     */
    std::string error_message;

    friend bool operator==(const job_instance_summary&, const job_instance_summary&) = default;
};

/**
 * @brief The page of executions.
 */
struct get_job_instances_response {
    bool success = true;
    std::string message;
    std::vector<job_instance_summary> instances;
    int total_available_count = 0;

    friend bool operator==(const get_job_instances_response&,
                           const get_job_instances_response&) = default;
};

/**
 * @brief Asks for the live status of every job the caller can see.
 *
 * It carries no fields, because the caller's session decides the scope and
 * the scheduler holds the rest.
 */
struct get_scheduler_status_request {
    using response_type = struct get_scheduler_status_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.ops.get_scheduler_status";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const get_scheduler_status_request&,
                           const get_scheduler_status_request&) = default;
};

/**
 * @brief One job's schedule and its last run, as the monitor renders it.
 */
struct job_schedule_status {
    std::string job_definition_id;
    std::string job_name;
    std::string description;
    /**
     * @brief The cron expression, as the job stores it.
     */
    std::string schedule_expression;
    bool is_active = false;
    /**
     * @brief When the job last started, in ISO-8601 UTC, if it ever has.
     */
    std::optional<std::string> last_run_at;
    /**
     * @brief The last run's state: =starting=, =succeeded= or =failed=.
     */
    std::optional<std::string> last_run_status;
    /**
     * @brief When the expression next fires, in ISO-8601 UTC.
     *
     * Absent for a paused job, and absent for an expression the evaluator
     * cannot advance.
     */
    std::optional<std::string> next_fire_at;
    /**
     * @brief Executions of this job that are still starting.
     */
    int running_count = 0;

    friend bool operator==(const job_schedule_status&, const job_schedule_status&) = default;
};

/**
 * @brief The status of every job, with the totals the monitor shows.
 */
struct get_scheduler_status_response {
    bool success = true;
    std::string message;
    std::vector<job_schedule_status> jobs;
    int total_running = 0;
    int total_active = 0;

    friend bool operator==(const get_scheduler_status_response&,
                           const get_scheduler_status_response&) = default;
};

/**
 * @brief Asks for every store the installation keeps, with its bound and size.
 *
 * The answer is one row per store and no paging: an installation keeps a dozen
 * stores, and the screen that reads this shows them all at once. The read is the
 * bounds view, so whether a store is bounded is a fact the database states rather
 * than an inference the screen makes.
 */
struct list_store_bounds_request {
    using response_type = struct list_store_bounds_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.store_bounds.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const list_store_bounds_request&,
                           const list_store_bounds_request&) = default;
};

/**
 * @brief What bounds the store, and whether that mechanism applies today.
 *
 * The mechanism is not the action. A policy may ask for an object expiry the
 * sweep cannot perform, and the store is then not bounded however clear its
 * policy reads.
 */
struct store_bound_action {
    /**
     * @brief What the bound does past its period, when it is a policy.
     */
    std::optional<std::string> action;
    /**
     * @brief The period the bound states, in days.
     */
    std::optional<int> retention_days;
    /**
     * @brief How many hours the bound waits between two applications of itself.
     *
     * It is the policy's own cadence when one bounds the store and the catalogue's
     * default when none does, so the form that creates a policy opens on the period
     * and the cadence the installation ships with.
     */
    std::optional<int> cadence_hours;
    /**
     * @brief The policy that bounds the store today, if one does.
     */
    std::optional<std::string> policy_code;
    /**
     * @brief Whether that policy is switched on.
     */
    bool policy_enabled = false;
    /**
     * @brief The version of that policy a write must still match.
     *
     * A change names the version it read, so a period somebody else moved in the
     * meantime is refused rather than overwritten. It is zero when no policy exists,
     * which is what a create claims: nothing is there yet.
     */
    int policy_version = 0;
    /**
     * @brief Why the store is kept for that period, in the policy's own words.
     *
     * A write carries it back unchanged, because a form that changes a period is not
     * the place that decides why the store is kept. When no policy bounds the store
     * it is the reason the catalogue ships with, so a person sees the words they are
     * about to adopt.
     */
    std::optional<std::string> policy_description;
    /**
     * @brief What bounds the store: =sweep=, =node_release=, =content_keying=,
     * =policy_off= or =none=.
     */
    std::string mechanism;
    /**
     * @brief Whether the mechanism the bound names exists and runs today.
     */
    bool bounded_now = false;

    friend bool operator==(const store_bound_action&, const store_bound_action&) = default;
};

/**
 * @brief What the bound last did, in its own words.
 *
 * A run that freed nothing is not a failure, and the detail line is what tells
 * the two apart; a screen that dropped it would have to guess.
 */
struct store_last_run {
    /**
     * @brief =applied=, =skipped= or =failed=.
     */
    std::optional<std::string> outcome;
    /**
     * @brief When it ran, in ISO-8601 UTC.
     */
    std::optional<std::string> ran_at;
    /**
     * @brief What it removed: chunks, rows or objects.
     */
    std::int64_t removed = 0;
    /**
     * @brief The run's own words.
     */
    std::optional<std::string> detail;

    friend bool operator==(const store_last_run&, const store_last_run&) = default;
};

/**
 * @brief What the store held when it was last measured.
 *
 * A store nothing measures answers with no sample rather than a size of zero, so
 * the absence of =sampled_at= is what tells an empty store from an unmeasured one.
 */
struct store_size {
    std::int64_t size_bytes = 0;
    std::int64_t measure_count = 0;
    /**
     * @brief When it was measured, in ISO-8601 UTC.
     */
    std::optional<std::string> sampled_at;

    friend bool operator==(const store_size&, const store_size&) = default;
};

/**
 * @brief One store, with what bounds it, what it last did and how large it is.
 */
struct store_bound {
    /**
     * @brief The store's catalogue code.
     */
    std::string code;
    /**
     * @brief The name a person reads.
     */
    std::string display_name;
    std::string description;
    std::string owner_component;
    std::string target_kind;
    std::string target;
    /**
     * @brief The column a row delete compares against the period, where one does.
     *
     * A store bounded by age is bounded on a column of its own, and the column is
     * what a person has to read to know which age the period means. A store the
     * database measures as a whole, or one whose bound lives in another component,
     * names no column.
     */
    std::optional<std::string> time_column;
    /**
     * @brief What the store is measured in: =relation_rows=, =objects= or =files=.
     *
     * The size is always bytes; this is the other number the sample carries, and
     * without it a count of twenty-seven says nothing about what was counted.
     */
    std::optional<std::string> measure_kind;
    /**
     * @brief The kind of bound the store ships with: =policy=, =keying= or =none=.
     */
    std::string bound_kind;
    store_bound_action bound;
    store_last_run last_run;
    store_size size;

    friend bool operator==(const store_bound&, const store_bound&) = default;
};

/**
 * @brief Every store, in catalogue order.
 */
struct list_store_bounds_response {
    bool success = true;
    std::string message;
    std::vector<store_bound> stores;

    friend bool operator==(const list_store_bounds_response&,
                           const list_store_bounds_response&) = default;
};

/**
 * @brief Asks for one store's measurements, oldest first, for the plot.
 */
struct list_store_samples_request {
    using response_type = struct list_store_samples_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.store_samples.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The store whose series the caller wants.
     */
    std::string store_code;
    /**
     * @brief How many of the most recent points to return.
     */
    std::uint32_t limit = 500;

    friend bool operator==(const list_store_samples_request&,
                           const list_store_samples_request&) = default;
};

/**
 * @brief One measurement of one store.
 */
struct store_sample {
    /**
     * @brief When the measurement was taken, in ISO-8601 UTC.
     */
    std::string sampled_at;
    std::int64_t size_bytes = 0;
    std::int64_t measure_count = 0;

    friend bool operator==(const store_sample&, const store_sample&) = default;
};

struct list_store_samples_response {
    bool success = true;
    std::string message;
    std::vector<store_sample> samples;

    friend bool operator==(const list_store_samples_response&,
                           const list_store_samples_response&) = default;
};

/**
 * @brief The members of get_job_instances_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_job_instances_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"offset", &get_job_instances_request::offset},
        ::ores::utility::codec::field{"limit", &get_job_instances_request::limit},
    };
}

/**
 * @brief The members of job_instance_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<job_instance_summary>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"id", &job_instance_summary::id},
        ::ores::utility::codec::field{"job_definition_id",
                                      &job_instance_summary::job_definition_id},
        ::ores::utility::codec::field{"job_name", &job_instance_summary::job_name},
        ::ores::utility::codec::field{"action_type", &job_instance_summary::action_type},
        ::ores::utility::codec::field{"status", &job_instance_summary::status},
        ::ores::utility::codec::field{"triggered_at", &job_instance_summary::triggered_at},
        ::ores::utility::codec::field{"started_at", &job_instance_summary::started_at},
        ::ores::utility::codec::field{"completed_at", &job_instance_summary::completed_at},
        ::ores::utility::codec::field{"duration_ms", &job_instance_summary::duration_ms},
        ::ores::utility::codec::field{"error_message", &job_instance_summary::error_message},
    };
}

/**
 * @brief The members of get_job_instances_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_job_instances_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_job_instances_response::success},
        ::ores::utility::codec::field{"message", &get_job_instances_response::message},
        ::ores::utility::codec::field{"instances", &get_job_instances_response::instances},
        ::ores::utility::codec::field{"total_available_count",
                                      &get_job_instances_response::total_available_count},
    };
}

/**
 * @brief The members of get_scheduler_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_scheduler_status_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of job_schedule_status, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<job_schedule_status>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"job_definition_id", &job_schedule_status::job_definition_id},
        ::ores::utility::codec::field{"job_name", &job_schedule_status::job_name},
        ::ores::utility::codec::field{"description", &job_schedule_status::description},
        ::ores::utility::codec::field{"schedule_expression",
                                      &job_schedule_status::schedule_expression},
        ::ores::utility::codec::field{"is_active", &job_schedule_status::is_active},
        ::ores::utility::codec::field{"last_run_at", &job_schedule_status::last_run_at},
        ::ores::utility::codec::field{"last_run_status", &job_schedule_status::last_run_status},
        ::ores::utility::codec::field{"next_fire_at", &job_schedule_status::next_fire_at},
        ::ores::utility::codec::field{"running_count", &job_schedule_status::running_count},
    };
}

/**
 * @brief The members of get_scheduler_status_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_scheduler_status_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_scheduler_status_response::success},
        ::ores::utility::codec::field{"message", &get_scheduler_status_response::message},
        ::ores::utility::codec::field{"jobs", &get_scheduler_status_response::jobs},
        ::ores::utility::codec::field{"total_running",
                                      &get_scheduler_status_response::total_running},
        ::ores::utility::codec::field{"total_active", &get_scheduler_status_response::total_active},
    };
}

/**
 * @brief The members of list_store_bounds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_store_bounds_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of store_bound_action, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_bound_action>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"action", &store_bound_action::action},
        ::ores::utility::codec::field{"retention_days", &store_bound_action::retention_days},
        ::ores::utility::codec::field{"cadence_hours", &store_bound_action::cadence_hours},
        ::ores::utility::codec::field{"policy_code", &store_bound_action::policy_code},
        ::ores::utility::codec::field{"policy_enabled", &store_bound_action::policy_enabled},
        ::ores::utility::codec::field{"policy_version", &store_bound_action::policy_version},
        ::ores::utility::codec::field{"policy_description",
                                      &store_bound_action::policy_description},
        ::ores::utility::codec::field{"mechanism", &store_bound_action::mechanism},
        ::ores::utility::codec::field{"bounded_now", &store_bound_action::bounded_now},
    };
}

/**
 * @brief The members of store_last_run, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_last_run>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"outcome", &store_last_run::outcome},
        ::ores::utility::codec::field{"ran_at", &store_last_run::ran_at},
        ::ores::utility::codec::field{"removed", &store_last_run::removed},
        ::ores::utility::codec::field{"detail", &store_last_run::detail},
    };
}

/**
 * @brief The members of store_size, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_size>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"size_bytes", &store_size::size_bytes},
        ::ores::utility::codec::field{"measure_count", &store_size::measure_count},
        ::ores::utility::codec::field{"sampled_at", &store_size::sampled_at},
    };
}

/**
 * @brief The members of store_bound, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_bound>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"code", &store_bound::code},
        ::ores::utility::codec::field{"display_name", &store_bound::display_name},
        ::ores::utility::codec::field{"description", &store_bound::description},
        ::ores::utility::codec::field{"owner_component", &store_bound::owner_component},
        ::ores::utility::codec::field{"target_kind", &store_bound::target_kind},
        ::ores::utility::codec::field{"target", &store_bound::target},
        ::ores::utility::codec::field{"time_column", &store_bound::time_column},
        ::ores::utility::codec::field{"measure_kind", &store_bound::measure_kind},
        ::ores::utility::codec::field{"bound_kind", &store_bound::bound_kind},
        ::ores::utility::codec::field{"bound", &store_bound::bound},
        ::ores::utility::codec::field{"last_run", &store_bound::last_run},
        ::ores::utility::codec::field{"size", &store_bound::size},
    };
}

/**
 * @brief The members of list_store_bounds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_store_bounds_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_store_bounds_response::success},
        ::ores::utility::codec::field{"message", &list_store_bounds_response::message},
        ::ores::utility::codec::field{"stores", &list_store_bounds_response::stores},
    };
}

/**
 * @brief The members of list_store_samples_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_store_samples_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"store_code", &list_store_samples_request::store_code},
        ::ores::utility::codec::field{"limit", &list_store_samples_request::limit},
    };
}

/**
 * @brief The members of store_sample, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_sample>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"sampled_at", &store_sample::sampled_at},
        ::ores::utility::codec::field{"size_bytes", &store_sample::size_bytes},
        ::ores::utility::codec::field{"measure_count", &store_sample::measure_count},
    };
}

/**
 * @brief The members of list_store_samples_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_store_samples_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_store_samples_response::success},
        ::ores::utility::codec::field{"message", &list_store_samples_response::message},
        ::ores::utility::codec::field{"samples", &list_store_samples_response::samples},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_job_instances_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_job_instances_request& v);
    static ores::scheduler::messaging::get_job_instances_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_job_instances_request& v);
    static ores::scheduler::messaging::get_job_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::job_instance_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::job_instance_summary& v);
    static ores::scheduler::messaging::job_instance_summary read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::job_instance_summary& v);
    static ores::scheduler::messaging::job_instance_summary
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_job_instances_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_job_instances_response& v);
    static ores::scheduler::messaging::get_job_instances_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_job_instances_response& v);
    static ores::scheduler::messaging::get_job_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_scheduler_status_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_scheduler_status_request& v);
    static ores::scheduler::messaging::get_scheduler_status_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_scheduler_status_request& v);
    static ores::scheduler::messaging::get_scheduler_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::job_schedule_status> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::job_schedule_status& v);
    static ores::scheduler::messaging::job_schedule_status read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::job_schedule_status& v);
    static ores::scheduler::messaging::job_schedule_status
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_scheduler_status_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_scheduler_status_response& v);
    static ores::scheduler::messaging::get_scheduler_status_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_scheduler_status_response& v);
    static ores::scheduler::messaging::get_scheduler_status_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_store_bounds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::list_store_bounds_request& v);
    static ores::scheduler::messaging::list_store_bounds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_store_bounds_request& v);
    static ores::scheduler::messaging::list_store_bounds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::store_bound_action> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::store_bound_action& v);
    static ores::scheduler::messaging::store_bound_action read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::store_bound_action& v);
    static ores::scheduler::messaging::store_bound_action
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::store_last_run> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::store_last_run& v);
    static ores::scheduler::messaging::store_last_run read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::store_last_run& v);
    static ores::scheduler::messaging::store_last_run read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::store_size> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::store_size& v);
    static ores::scheduler::messaging::store_size read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::store_size& v);
    static ores::scheduler::messaging::store_size read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::store_bound> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::store_bound& v);
    static ores::scheduler::messaging::store_bound read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::store_bound& v);
    static ores::scheduler::messaging::store_bound read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_store_bounds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::list_store_bounds_response& v);
    static ores::scheduler::messaging::list_store_bounds_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_store_bounds_response& v);
    static ores::scheduler::messaging::list_store_bounds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_store_samples_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::list_store_samples_request& v);
    static ores::scheduler::messaging::list_store_samples_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_store_samples_request& v);
    static ores::scheduler::messaging::list_store_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::store_sample> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::store_sample& v);
    static ores::scheduler::messaging::store_sample read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::store_sample& v);
    static ores::scheduler::messaging::store_sample read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_store_samples_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::list_store_samples_response& v);
    static ores::scheduler::messaging::list_store_samples_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_store_samples_response& v);
    static ores::scheduler::messaging::list_store_samples_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
