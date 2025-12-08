#include "ores.eventing/generators/entity_change_event_generator.hpp"

#include <faker-cxx/date.h>
#include <faker-cxx/word.h>
// #include "ores.utility/datetime/datetime.hpp"

namespace ores::eventing::generators {

ores::eventing::domain::entity_change_event entity_change_event_generator::generate() {
    ores::eventing::domain::entity_change_event n;
    n.entity = std::string(faker::word::noun()) + "." + std::string(faker::word::noun());
    std::string date_str = std::string(faker::date::pastDate());
    // using utility::datetime::datetime;
    // n.timestamp = date_str;
    // FIXME

    return n;
}

std::vector<ores::eventing::domain::entity_change_event>
entity_change_event_generator::generate_set(size_t n) {
    std::vector<domain::entity_change_event> r;
    r.reserve(n);
    for (size_t i = 0; i < n; ++i)
        r.push_back(generate());

    return r;
}

}
