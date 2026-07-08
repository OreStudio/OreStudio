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
#ifndef ORES_QT_CURRENCY_DETAIL_DIALOG_HPP
#define ORES_QT_CURRENCY_DETAIL_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/SettingGatedActionController.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/currency_history_protocol.hpp"
#include <QAction>
#include <QToolBar>
#include <vector>


namespace Ui {
class CurrencyDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing currency records.
 *
 * This dialog allows viewing, creating, and editing currencies.
 * It supports both create mode (for new records) and edit mode (for
 * existing records).
 */
class CurrencyDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.currency_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CurrencyDetailDialog(QWidget* parent = nullptr);
    ~CurrencyDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setCurrency(const refdata::domain::currency& currency);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly, int versionNumber = 0);

    /**
     * @brief Sets the history for version navigation.
     *
     * Shows a navigation toolbar allowing the user to navigate between
     * versions (first/prev/next/last); the flag is grayed out for
     * non-latest versions.
     *
     * @param history All versions ordered newest-first (index 0 is latest)
     * @param versionNumber The version number to initially display
     */
    void setHistory(const refdata::messaging::currency_version_history& history, int versionNumber);

    /**
     * @brief Force the dialog into the unsaved-changes state.
     *
     * Used when values are loaded programmatically and must be savable
     * immediately even though the user typed nothing — e.g. a revert, where
     * the act of loading a past version's values is itself the change.
     */
    void markDirty();


signals:
    void currencySaved(const QString& code);
    void currencyDeleted(const QString& code);

    /**
     * @brief Emitted when user requests to revert to the displayed historical version.
     * @param currency The currency data to revert to.
     */
    void revertRequested(const refdata::domain::currency& currency);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onCodeChanged(const QString& text);
    void onFieldChanged();
    void onRevertClicked();
    void onFirstVersionClicked();
    void onPrevVersionClicked();
    void onNextVersionClicked();
    void onLastVersionClicked();
    void onGenerateClicked();

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override {
        return hasChanges_;
    }
    QString code() const override;
    std::optional<boost::uuids::uuid> entityImageId() const override;
    QLineEdit* keyFlagField() const override;
    QIcon keyFlagIcon(const std::string& key) const override;

private:
    void setupUi();
    void setupConnections();
    void setupCombos();
    void setupGenerateAction();
    void updateUiFromCurrency();
    void updateCurrencyFromUi();
    void updateSaveButtonState();
    bool validateInput();

    void populateMonetaryNatureCombo();

    void populateMarketTierCombo();

    void populateRoundingTypeCombo();


    void displayCurrentVersion();
    void updateVersionNavButtonStates();
    void showVersionNavActions(bool visible);

    Ui::CurrencyDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    refdata::domain::currency currency_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};

    QAction* generateAction_;
    SettingGatedActionController* settingGatedActions_{nullptr};
    QToolBar* toolBar_{nullptr};
    QAction* revertAction_{nullptr};
    QAction* firstVersionAction_{nullptr};
    QAction* prevVersionAction_{nullptr};
    QAction* nextVersionAction_{nullptr};
    QAction* lastVersionAction_{nullptr};
    refdata::messaging::currency_version_history history_;
    int currentHistoryIndex_{0};
    int historicalVersion_{0};
};

}

#endif
