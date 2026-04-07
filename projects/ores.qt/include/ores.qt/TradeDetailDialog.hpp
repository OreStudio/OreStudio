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
#ifndef ORES_QT_TRADE_DETAIL_DIALOG_HPP
#define ORES_QT_TRADE_DETAIL_DIALOG_HPP

#include <map>
#include <vector>
#include <QTabWidget>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.qt/IInstrumentForm.hpp"
#include "ores.qt/InstrumentFormRegistry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_type.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"

namespace Ui {
class TradeDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing a trade and its linked
 *        instrument.
 *
 * The dialog contains trade tabs (General, Dates) followed by
 * instrument-family tabs that are revealed once the instrument loads
 * asynchronously. A single Provenance tab holds two sections — Trade and
 * Instrument — so that both audit trails are visible in one place.
 *
 * Save semantics: if the instrument is dirty, it is saved first; the trade
 * is then always saved so that any instrument change is reflected in the
 * trade version number (downstream notification invariant).
 */
class TradeDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.trade_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TradeDetailDialog(QWidget* parent = nullptr);
    ~TradeDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setTrade(const trading::domain::trade& trade);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

protected:
    QTabWidget*       tabWidget()        const override;
    QWidget*          provenanceTab()    const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override {
        return hasChanges_ || instrumentHasChanges_;
    }

signals:
    void tradeSaved(const QString& code);
    void tradeDeleted(const QString& code);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onCodeChanged(const QString& text);
    void onFieldChanged();
    void onInstrumentFieldChanged();
    void onCreateTradeTypeChanged(const QString& text);

private:
    void setupUi();
    void setupConnections();
    void loadBooks();
    void selectCurrentBook();
    void loadCounterparties();
    void selectCurrentCounterparty();
    void loadTradeTypes();
    void updateUiFromTrade();
    void updateTradeFromUi();
    void updateSaveButtonState();
    bool validateInput();
    void activateForm(IInstrumentForm* form, const std::string& tradeTypeCode);

    void saveTrade(const trading::domain::trade& trade);

    Ui::TradeDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    trading::domain::trade trade_;
    std::vector<refdata::domain::book> books_;
    std::vector<refdata::domain::counterparty> counterparties_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};

    // All IInstrumentForm pages live in instrumentStack; formMap_ gives O(1)
    // access by product_type without walking the stack at runtime.
    InstrumentFormRegistry instrumentFormRegistry_;
    std::map<trading::domain::product_type, IInstrumentForm*> formMap_;
    IInstrumentForm* activeForm_ = nullptr;

    // Trade-type reference data cached on connect for flag lookups.
    std::map<std::string, trading::domain::trade_type> tradeTypeCache_;

    bool instrumentLoaded_{false};
    bool instrumentHasChanges_{false};
};

}

#endif
