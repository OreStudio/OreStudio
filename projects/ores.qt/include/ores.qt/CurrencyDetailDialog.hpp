/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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

#include <QToolBar>
#include <QAction>
#include <QPushButton>
#include <memory>
#include <vector>
#include "ores.refdata/domain/currency.hpp"
#include "ores.refdata/domain/currency_version_history.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.logging/make_logger.hpp"

namespace Ui {

class CurrencyDetailDialog;

}

namespace ores::qt {

class ChangeReasonCache;

class CurrencyDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.currency_detail_dialog";

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
    void setImageCache(ImageCache* imageCache);
    void setChangeReasonCache(ChangeReasonCache* changeReasonCache);

    void setCurrency(const refdata::domain::currency& currency);
    [[nodiscard]] refdata::domain::currency getCurrency() const;
    void clearDialog();
    void save();

    /**
     * @brief Sets the dialog to read-only mode for viewing historical versions.
     *
     * In read-only mode:
     * - All fields are disabled
     * - Save button is hidden
     * - Delete button is hidden
     * - Revert button is shown
     * - Toolbar shows version information
     *
     * @param readOnly True to enable read-only mode
     * @param versionNumber The historical version number being displayed
     */
    void setReadOnly(bool readOnly, int versionNumber = 0);

    /**
     * @brief Sets the history for version navigation.
     *
     * When set, shows a navigation toolbar allowing the user to navigate
     * between versions (first/prev/next/last). The flag is grayed out
     * for non-latest versions.
     *
     * @param history All versions ordered newest-first (index 0 is latest)
     * @param versionNumber The version number to initially display
     */
    void setHistory(const refdata::domain::currency_version_history& history,
        int versionNumber);

    /**
     * @brief Mark the dialog data as stale.
     *
     * Called when a notification is received indicating this currency has
     * changed on the server. Shows a visual indicator that the data may be
     * out of date.
     */
    void markAsStale();

    /**
     * @brief Returns the ISO code of the currency being edited.
     */
    [[nodiscard]] QString isoCode() const;

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override { return isDirty_; }

signals:
    void currencyUpdated(const QString& iso_code);
    void currencyCreated(const QString& iso_code);
    void currencyDeleted(const QString& iso_code);
    void isDirtyChanged(bool isDirty);
    void showRoundingTypesRequested();
    void showMonetaryNaturesRequested();
    void showMarketTiersRequested();

    /**
     * @brief Emitted when user requests to revert to the displayed historical version.
     * @param currency The currency data to revert to.
     */
    void revertRequested(const refdata::domain::currency& currency);

private slots:
    void onSaveClicked();
    void onResetClicked();
    void onDeleteClicked();
    void onRevertClicked();
    void onFieldChanged();
    void onSelectFlagClicked();
    void onCurrencyImageSet(const QString& iso_code, bool success, const QString& message);
    void onGenerateClicked();

    // Version navigation slots
    void onFirstVersionClicked();
    void onPrevVersionClicked();
    void onNextVersionClicked();
    void onLastVersionClicked();

    void onFeatureFlagNotification(const QString& eventType, const QDateTime& timestamp,
        const QStringList& entityIds);
    void onConnectionEstablished();

private:
    void updateSaveResetButtonState();
    void setFieldsReadOnly(bool readOnly);
    void updateFlagDisplay();
    void displayCurrentVersion();
    void updateVersionNavButtonStates();
    void showVersionNavActions(bool visible);
    void setupGenerateAction();
    void updateGenerateActionVisibility();
    void populateRoundingTypeCombo();
    void populateMonetaryNatureCombo();
    void populateMarketTierCombo();

private:
    std::unique_ptr<Ui::CurrencyDetailDialog> ui_;
    bool isDirty_;
    bool isAddMode_;
    bool isReadOnly_;
    bool isStale_;
    bool flagChanged_;
    int historicalVersion_;
    std::string username_;
    QToolBar* toolBar_;
    QAction* revertAction_;
    QAction* generateAction_;
    QPushButton* flagButton_;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;
    QAction* isoCodeFlagAction_{nullptr};
    refdata::domain::currency currentCurrency_;
    QString pendingImageId_;
    static constexpr const char* max_timestamp = "9999-12-31 23:59:59";

    // Version navigation members
    refdata::domain::currency_version_history history_;
    int currentHistoryIndex_;
    QAction* firstVersionAction_;
    QAction* prevVersionAction_;
    QAction* nextVersionAction_;
    QAction* lastVersionAction_;
};

}

#endif
