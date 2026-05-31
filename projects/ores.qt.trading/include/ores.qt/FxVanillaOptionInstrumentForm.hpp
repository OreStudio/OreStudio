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
#ifndef ORES_QT_FX_VANILLA_OPTION_INSTRUMENT_FORM_HPP
#define ORES_QT_FX_VANILLA_OPTION_INSTRUMENT_FORM_HPP

#include "ores.qt/IInstrumentForm.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"

namespace Ui {
class FxVanillaOptionInstrumentForm;
}

namespace ores::qt {

/**
 * @brief @c IInstrumentForm subclass for FX Vanilla Option (FxOption) trades.
 *
 * Handles European and American FX options. Registered in
 * @c InstrumentFormRegistry under the trade type code @c "FxOption".
 */
class FxVanillaOptionInstrumentForm final : public IInstrumentForm {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.fx_vanilla_option_instrument_form";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit FxVanillaOptionInstrumentForm(QWidget* parent = nullptr);
    ~FxVanillaOptionInstrumentForm() override;

    void setClientManager(ClientManager* cm) override;
    void setUsername(const std::string& username) override;
    void setImageCache(ImageCache* cache) override;

    void populate(const trading::domain::fx_vanilla_option_instrument& instr) override;
    void clear() override;

    void setTradeType(const QString& code,
        bool has_options, bool has_extension) override;

    void setReadOnly(bool readOnly) override;
    bool isDirty() const override;
    bool isLoaded() const override;

    void setChangeReason(
        const std::string& code, const std::string& commentary) override;
    void writeUiToInstrument() override;

    void saveInstrument(
        std::function<void(const std::string& id)> on_success,
        std::function<void(const QString& error)> on_failure) override;

private:
    void setupConnections();
    void populateCurrencies();
    void populateFromInstrument();
    void emitProvenance();
    void onFieldChanged();

    Ui::FxVanillaOptionInstrumentForm* ui_;
    ClientManager* clientManager_ = nullptr;
    ImageCache* imageCache_ = nullptr;
    std::string username_;
    trading::domain::fx_vanilla_option_instrument instrument_;
    bool dirty_ = false;
    bool loaded_ = false;
};

}

#endif
