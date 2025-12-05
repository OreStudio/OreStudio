#include "ores.notification/generators/notification_generator.hpp"

#include <faker-cxx/date.h>
#include <faker-cxx/word.h>
#include <string>
#include <chrono>
#include <sstream>
#include <iomanip> // For std::get_time
#include <ctime>   // For std::mktime

namespace ores::notification::generators {

// Helper function to parse "YYYY-MM-DD" string to std::chrono::system_clock::time_point
// This assumes the date from faker is in a format compatible with %Y-%m-%d
std::chrono::system_clock::time_point parse_yyyymmdd_to_time_point(const std::string& date_str) {
    std::tm tm = {};
    std::stringstream ss(date_str);
    ss >> std::get_time(&tm, "%Y-%m-%d"); // Parse as local time initially
    if (ss.fail()) {
        // Fallback or error handling; returning epoch for now
        return std::chrono::system_clock::time_point{};
    }
    // Convert tm to time_t, then to system_clock::time_point.
    // std::mktime interprets tm in local time, which might be an issue.
    // For simplicity with faker's YYYY-MM-DD, assuming local timezone for generation is acceptable
    // within a test generator context, or we need a more robust UTC parsing solution.
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}


ores::notification::domain::notification notification_generator::generate() {
    ores::notification::domain::notification n;
    n.entity = std::string("ores.risk.") + std::string(faker::word::noun());
    // Generate a date string and convert it
    std::string date_str = std::string(faker::date::pastDate());
    n.timestamp = parse_yyyymmdd_to_time_point(date_str);

    return n;
}

std::vector<ores::notification::domain::notification> notification_generator::generate_set(size_t n) {
    std::vector<ores::notification::domain::notification> notifications;
    notifications.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        notifications.push_back(generate());
    }
    return notifications;
}

} // namespace ores::notification::generators
