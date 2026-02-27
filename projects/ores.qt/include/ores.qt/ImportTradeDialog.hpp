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
#ifndef ORES_QT_IMPORT_TRADE_DIALOG_HPP
#define ORES_QT_IMPORT_TRADE_DIALOG_HPP

#include <atomic>
#include <vector>
#include <QString>
#include <QDialog>
#include <QLabel>
#include <QWidget>
#include <QCheckBox>
#include <QDateEdit>
#include <QLineEdit>
#include <QPushButton>
#include <QComboBox>
#include <QTableWidget>
#include <QProgressBar>
#include <QFutureWatcher>
#include <boost/uuid/uuid.hpp>
#include "ores.ore/xml/importer.hpp"
#include "ores.refdata/domain/book.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Dialog for reviewing defaults and importing ORE portfolio trades.
 *
 * Shows an "Import Defaults" section for setting trade dates, lifecycle event,
 * default netting set, and default counterparty. A trade preview table shows
 * each trade with a per-row counterparty combo box and editable netting set
 * field, allowing individual overrides before import.
 *
 * ORES counterparties are fetched asynchronously when the dialog opens.
 * Trades can be imported without counterparty assignments if none are mapped.
 */
class ImportTradeDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.import_trade_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the import dialog.
     *
     * @param book The ORES book trades will be imported into
     * @param items Trades to import (each item carries its own source_file)
     * @param source_label Human-readable label shown at the top of the dialog
     * @param clientManager Client manager for server communication
     * @param username Current logged-in username
     * @param parent Parent widget
     */
    explicit ImportTradeDialog(
        const refdata::domain::book& book,
        const std::vector<ore::xml::trade_import_item>& items,
        const QString& source_label,
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);

    ~ImportTradeDialog() override;

signals:
    /**
     * @brief Emitted when import completes.
     * @param success_count Number of trades successfully imported
     * @param total_count Total number of trades attempted
     */
    void importCompleted(int success_count, int total_count);

    /**
     * @brief Emitted when import is cancelled.
     */
    void importCancelled();

private slots:
    void onImportClicked();
    void onCancelClicked();
    void onSelectAllChanged(Qt::CheckState state);
    void onTradeCheckChanged();
    void onApplyNettingSetToAll();
    void onApplyCounterpartyToAll();

private:
    void setupUI();
    void populateTradeTable();
    void populateCounterpartyCombos();
    void loadCounterparties();
    void updateImportButtonState();
    void updateSelectionCount();

private:
    refdata::domain::book book_;
    std::vector<ore::xml::trade_import_item> items_;
    std::vector<std::string> validation_errors_;
    std::vector<refdata::domain::counterparty> counterparties_;
    QString source_label_;
    ClientManager* clientManager_;
    QString username_;

    // Import defaults
    QDateEdit* tradeDateEdit_;
    QDateEdit* effectiveDateEdit_;
    QDateEdit* terminationDateEdit_;
    QComboBox* lifecycleEventCombo_;
    QLineEdit* defaultNettingSetEdit_;
    QComboBox* defaultCounterpartyCombo_;
    QLabel* counterpartyStatusLabel_;

    // Trade table and controls
    QLabel* fileLabel_;
    QLabel* bookLabel_;
    QCheckBox* selectAllCheckbox_;
    QLabel* selectionCountLabel_;
    QTableWidget* tradeTable_;

    // Progress and status
    QProgressBar* progressBar_;
    QLabel* statusLabel_;
    QPushButton* importButton_;
    QPushButton* cancelButton_;

    bool importInProgress_;
    std::atomic<bool> cancelRequested_;
};

}

#endif
