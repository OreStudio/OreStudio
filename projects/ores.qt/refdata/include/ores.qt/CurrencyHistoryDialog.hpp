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
#ifndef ORES_QT_CURRENCY_HISTORY_DIALOG_HPP
#define ORES_QT_CURRENCY_HISTORY_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include <QString>
#include <memory>

namespace Ui {
class CurrencyHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Widget for displaying currency version history.
 */
class CurrencyHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.currency_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CurrencyHistoryDialog(QString iso_code,
                                   ClientManager* clientManager,
                                   QWidget* parent = nullptr);
    ~CurrencyHistoryDialog() override;

    void loadHistory() override;

    /**
     * @brief Set the image cache for displaying currency flags.
     */
    void setImageCache(ImageCache* imageCache);

    /**
     * @brief Returns the ISO code of the currency.
     */
    [[nodiscard]] QString code() const override {
        return isoCode_;
    }

    /**
     * @brief Returns the loaded history for version navigation.
     */
    [[nodiscard]] const refdata::messaging::currency_version_history& getHistory() const {
        return history_;
    }

protected:
    void closeEvent(QCloseEvent* event) override;

signals:
    /**
     * @brief Emitted when user requests to open a version in read-only mode.
     * @param currency The currency data at the selected version.
     * @param versionNumber The version number being viewed.
     */
    void openVersionRequested(const refdata::domain::currency& currency, int versionNumber);

    /**
     * @brief Emitted when user requests to revert to a selected version.
     * @param currency The currency data to revert to.
     */
    void revertVersionRequested(const refdata::domain::currency& currency);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult calculateDiffAt(int current_index, int previous_index) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;
    QWidget* changeCellWidget(const QString& field, const QString& value) override;

private:
    std::unique_ptr<Ui::CurrencyHistoryDialog> ui_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString isoCode_;
    refdata::messaging::currency_version_history history_;
};

}

#endif
