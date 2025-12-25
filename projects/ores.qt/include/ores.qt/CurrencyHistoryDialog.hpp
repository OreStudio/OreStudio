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

#include <memory>
#include <QPair>
#include <QWidget>
#include <QString>
#include <QVector>
#include <QToolBar>
#include <QAction>
#include "ores.qt/ClientManager.hpp"
#include "ores.risk/domain/currency_version.hpp"
#include "ores.risk/domain/currency_version_history.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ui_CurrencyHistoryDialog.h"

namespace Ui {
class CurrencyHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Widget for displaying currency version history.
 */
class CurrencyHistoryDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.currency_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    const QIcon& getHistoryIcon() const;

public:
    explicit CurrencyHistoryDialog(QString iso_code,
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~CurrencyHistoryDialog() override;

    void loadHistory();

    QSize sizeHint() const override; // Provide optimal size based on table content

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

    /**
     * @brief Emitted when user requests to open a version in read-only mode.
     * @param currency The currency data at the selected version.
     * @param versionNumber The version number being viewed.
     */
    void openVersionRequested(const risk::domain::currency& currency, int versionNumber);

    /**
     * @brief Emitted when user requests to revert to a selected version.
     * @param currency The currency data to revert to.
     */
    void revertVersionRequested(const risk::domain::currency& currency);

private slots:
    void onVersionSelected(int index);
    void onHistoryLoaded();
    void onHistoryLoadError(const QString& error);
    void onOpenClicked();
    void onRevertClicked();

private:
    void displayChangesTab(int version_index);
    void displayFullDetailsTab(int version_index);

    /**
     * @brief Calculate differences between two versions.
     *
     * @return Vector of (field_name, (old_value, new_value)) pairs.
     */
    using DiffResult = QVector<QPair<QString, QPair<QString, QString>>>;
    DiffResult calculateDiff(
        const risk::domain::currency_version& current,
        const risk::domain::currency_version& previous);

    void setupToolbar();
    void updateButtonStates();
    int selectedVersionIndex() const;

    std::unique_ptr<Ui::CurrencyHistoryDialog> ui_;
    ClientManager* clientManager_;
    QString isoCode_;
    risk::domain::currency_version_history history_;

    QToolBar* toolBar_;
    QAction* openAction_;
    QAction* revertAction_;
};

}

#endif
