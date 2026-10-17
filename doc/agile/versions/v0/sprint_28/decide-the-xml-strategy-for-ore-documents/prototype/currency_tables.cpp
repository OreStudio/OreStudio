// Generated from the ORE schema. Do not edit by hand.
#include "codec.hpp"

namespace ore::xml {
namespace {

const char* rounding_type_spellings[] = {"Up", "Down", "Closest", "Floor", "Ceiling", nullptr};

const field currency_row_fields[] = {
    {"Name", detail::hash_name("Name"), value_kind::text, offsetof(currency_row, name), 0, nullptr},
    {"ISOCode", detail::hash_name("ISOCode"), value_kind::text, offsetof(currency_row, iso_code), 1, nullptr},
    {"MinorUnitCodes", detail::hash_name("MinorUnitCodes"), value_kind::text, offsetof(currency_row, minor_unit_codes), 2, nullptr},
    {"NumericCode", detail::hash_name("NumericCode"), value_kind::integer, offsetof(currency_row, numeric_code), 3, nullptr},
    {"Symbol", detail::hash_name("Symbol"), value_kind::text, offsetof(currency_row, symbol), 4, nullptr},
    {"FractionSymbol", detail::hash_name("FractionSymbol"), value_kind::text, offsetof(currency_row, fraction_symbol), 5, nullptr},
    {"FractionsPerUnit", detail::hash_name("FractionsPerUnit"), value_kind::integer, offsetof(currency_row, fractions_per_unit), 6, nullptr},
    {"RoundingType", detail::hash_name("RoundingType"), value_kind::enumeration, offsetof(currency_row, rounding_type), 7, rounding_type_spellings},
    {"RoundingPrecision", detail::hash_name("RoundingPrecision"), value_kind::integer, offsetof(currency_row, rounding_precision), 8, nullptr},
    {"CurrencyType", detail::hash_name("CurrencyType"), value_kind::text, offsetof(currency_row, currency_type), 9, nullptr},
};

const field currency_view_fields[] = {
    {"Name", detail::hash_name("Name"), value_kind::text, offsetof(currency_view, name), 0, nullptr},
    {"ISOCode", detail::hash_name("ISOCode"), value_kind::text, offsetof(currency_view, iso_code), 1, nullptr},
    {"MinorUnitCodes", detail::hash_name("MinorUnitCodes"), value_kind::text, offsetof(currency_view, minor_unit_codes), 2, nullptr},
    {"NumericCode", detail::hash_name("NumericCode"), value_kind::integer, offsetof(currency_view, numeric_code), 3, nullptr},
    {"Symbol", detail::hash_name("Symbol"), value_kind::text, offsetof(currency_view, symbol), 4, nullptr},
    {"FractionSymbol", detail::hash_name("FractionSymbol"), value_kind::text, offsetof(currency_view, fraction_symbol), 5, nullptr},
    {"FractionsPerUnit", detail::hash_name("FractionsPerUnit"), value_kind::integer, offsetof(currency_view, fractions_per_unit), 6, nullptr},
    {"RoundingType", detail::hash_name("RoundingType"), value_kind::enumeration, offsetof(currency_view, rounding_type), 7, rounding_type_spellings},
    {"RoundingPrecision", detail::hash_name("RoundingPrecision"), value_kind::integer, offsetof(currency_view, rounding_precision), 8, nullptr},
    {"CurrencyType", detail::hash_name("CurrencyType"), value_kind::text, offsetof(currency_view, currency_type), 9, nullptr},
};

const record_type row_record = {"currencyDefinition", currency_row_fields, 10};
const document_type row_document = {"CurrencyConfig", "Currency", &row_record};
const record_type view_record = {"currencyDefinition", currency_view_fields, 10};
const document_type view_document = {"CurrencyConfig", "Currency", &view_record};

} // namespace

auto currency_document() -> const document_type& {
    return row_document;
}

auto currency_view_document() -> const document_type& {
    return view_document;
}

auto write_currencies(const std::vector<currency_row>& rows, std::string& out) -> void {
    detail::write_items<detail::materialized>(rows, row_document, out);
}

} // namespace ore::xml
