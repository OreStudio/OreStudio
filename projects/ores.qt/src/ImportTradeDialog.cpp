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
#include "ores.qt/ImportTradeDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QHeaderView>
#include <QDateTime>
#include <QPointer>
#include <QtConcurrent/QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.ore/xml/importer.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ImportTradeDialog::ImportTradeDialog(
    const refdata::domain::book& book,
    const std::vector<ore::xml::trade_import_item>& items,
    const QString& source_label,
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : QDialog(parent),
      book_(book),
      items_(items),
      source_label_(source_label),
      clientManager_(clientManager),
      username_(username),
      tradeDateEdit_(nullptr),
      effectiveDateEdit_(nullptr),
      terminationDateEdit_(nullptr),
      lifecycleEventCombo_(nullptr),
      defaultNettingSetEdit_(nullptr),
      defaultCounterpartyCombo_(nullptr),
      counterpartyStatusLabel_(nullptr),
      fileLabel_(nullptr),
      bookLabel_(nullptr),
      selectAllCheckbox_(nullptr),
      selectionCountLabel_(nullptr),
      tradeTable_(nullptr),
      progressBar_(nullptr),
      statusLabel_(nullptr),
      importButton_(nullptr),
      cancelButton_(nullptr),
      importInProgress_(false),
      cancelRequested_(false) {

    BOOST_LOG_SEV(lg(), debug) << "Creating import trade dialog for book: "
                               << book.name << " with " << items.size()
                               << " trades from: "
                               << source_label.toStdString();

    validation_errors_.reserve(items.size());
    for (const auto& item : items) {
        validation_errors_.push_back(
            ore::xml::importer::validate_trade(item.trade));
    }

    setupUI();
    populateTradeTable();
    updateSelectionCount();

    loadCounterparties();
    setAttribute(Qt::WA_DeleteOnClose);
}

ImportTradeDialog::~ImportTradeDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying import trade dialog";
}

void ImportTradeDialog::setupUI() {
    setWindowTitle("Import ORE Portfolio Trades");
    setModal(true);
    resize(980, 760);

    auto* mainLayout = new QVBoxLayout(this);

    fileLabel_ = new QLabel(QString("Source: %1").arg(source_label_));
    mainLayout->addWidget(fileLabel_);

    const QString bookStr = QString("Book: %1")
        .arg(QString::fromStdString(book_.name));
    bookLabel_ = new QLabel(bookStr);
    mainLayout->addWidget(bookLabel_);

    // Import defaults group
    auto* defaultsGroup = new QGroupBox(tr("Import Defaults"), this);
    auto* defaultsGrid = new QGridLayout(defaultsGroup);

    // Row 0: dates
    tradeDateEdit_ = new QDateEdit(QDate::currentDate());
    tradeDateEdit_->setCalendarPopup(true);
    tradeDateEdit_->setDisplayFormat("yyyy-MM-dd");

    effectiveDateEdit_ = new QDateEdit(QDate::currentDate());
    effectiveDateEdit_->setCalendarPopup(true);
    effectiveDateEdit_->setDisplayFormat("yyyy-MM-dd");

    terminationDateEdit_ = new QDateEdit(QDate::currentDate().addYears(1));
    terminationDateEdit_->setCalendarPopup(true);
    terminationDateEdit_->setDisplayFormat("yyyy-MM-dd");

    defaultsGrid->addWidget(new QLabel(tr("Trade Date:")), 0, 0);
    defaultsGrid->addWidget(tradeDateEdit_, 0, 1);
    defaultsGrid->addWidget(new QLabel(tr("Effective Date:")), 0, 2);
    defaultsGrid->addWidget(effectiveDateEdit_, 0, 3);
    defaultsGrid->addWidget(new QLabel(tr("Termination Date:")), 0, 4);
    defaultsGrid->addWidget(terminationDateEdit_, 0, 5);

    // Row 1: lifecycle event
    lifecycleEventCombo_ = new QComboBox();
    lifecycleEventCombo_->addItems({
        "New", "Amendment", "Novation",
        "Full Termination", "Partial Termination",
        "Exercise", "Maturity", "Withdrawal"
    });
    defaultsGrid->addWidget(new QLabel(tr("Lifecycle Event:")), 1, 0);
    defaultsGrid->addWidget(lifecycleEventCombo_, 1, 1);

    // Row 2: default netting set
    defaultNettingSetEdit_ = new QLineEdit();
    defaultNettingSetEdit_->setPlaceholderText(tr("e.g. NS_CPTY_A"));
    auto* applyNsBtn = new QPushButton(tr("Apply to All"));
    connect(applyNsBtn, &QPushButton::clicked,
            this, &ImportTradeDialog::onApplyNettingSetToAll);
    auto* nsRowWidget = new QWidget();
    auto* nsRowLayout = new QHBoxLayout(nsRowWidget);
    nsRowLayout->setContentsMargins(0, 0, 0, 0);
    nsRowLayout->addWidget(defaultNettingSetEdit_);
    nsRowLayout->addWidget(applyNsBtn);
    nsRowLayout->addStretch();
    defaultsGrid->addWidget(new QLabel(tr("Default Netting Set:")), 2, 0);
    defaultsGrid->addWidget(nsRowWidget, 2, 1, 1, 5);

    // Row 3: default counterparty
    defaultCounterpartyCombo_ = new QComboBox();
    defaultCounterpartyCombo_->addItem(tr("-- None --"), QString());
    defaultCounterpartyCombo_->setMinimumWidth(220);
    auto* applyCpBtn = new QPushButton(tr("Apply to All"));
    connect(applyCpBtn, &QPushButton::clicked,
            this, &ImportTradeDialog::onApplyCounterpartyToAll);
    counterpartyStatusLabel_ = new QLabel(tr("Loading..."));
    counterpartyStatusLabel_->setStyleSheet("color: gray; font-style: italic;");
    auto* cpRowWidget = new QWidget();
    auto* cpRowLayout = new QHBoxLayout(cpRowWidget);
    cpRowLayout->setContentsMargins(0, 0, 0, 0);
    cpRowLayout->addWidget(defaultCounterpartyCombo_);
    cpRowLayout->addWidget(applyCpBtn);
    cpRowLayout->addWidget(counterpartyStatusLabel_);
    cpRowLayout->addStretch();
    defaultsGrid->addWidget(new QLabel(tr("Default Counterparty:")), 3, 0);
    defaultsGrid->addWidget(cpRowWidget, 3, 1, 1, 5);

    mainLayout->addWidget(defaultsGroup);

    // Trades section
    auto* tradesGroup = new QGroupBox(tr("Trades to Import"), this);
    auto* tradesLayout = new QVBoxLayout(tradesGroup);

    auto* selectionLayout = new QHBoxLayout();
    selectAllCheckbox_ = new QCheckBox(tr("Select All"));
    selectAllCheckbox_->setChecked(true);
    connect(selectAllCheckbox_, &QCheckBox::checkStateChanged,
            this, &ImportTradeDialog::onSelectAllChanged);
    selectionLayout->addWidget(selectAllCheckbox_);

    selectionCountLabel_ = new QLabel();
    selectionLayout->addWidget(selectionCountLabel_);
    selectionLayout->addStretch();
    tradesLayout->addLayout(selectionLayout);

    tradeTable_ = new QTableWidget(this);
    tradeTable_->setColumnCount(5);
    tradeTable_->setHorizontalHeaderLabels({
        "", "External ID", "Trade Type", "Counterparty", "Netting Set ID"
    });
    tradeTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tradeTable_->setEditTriggers(
        QAbstractItemView::DoubleClicked | QAbstractItemView::AnyKeyPressed);
    tradeTable_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    tradeTable_->setColumnWidth(0, 30);
    tradeTable_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    tradeTable_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    tradeTable_->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
    tradeTable_->horizontalHeader()->setSectionResizeMode(4, QHeaderView::Stretch);
    tradeTable_->verticalHeader()->setVisible(false);
    tradesLayout->addWidget(tradeTable_);

    mainLayout->addWidget(tradesGroup);

    // Progress bar
    progressBar_ = new QProgressBar(this);
    progressBar_->setVisible(false);
    progressBar_->setTextVisible(true);
    mainLayout->addWidget(progressBar_);

    // Status label
    statusLabel_ = new QLabel();
    statusLabel_->setVisible(false);
    mainLayout->addWidget(statusLabel_);

    // Buttons
    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch();

    importButton_ = new QPushButton(tr("Import"));
    connect(importButton_, &QPushButton::clicked,
            this, &ImportTradeDialog::onImportClicked);
    buttonLayout->addWidget(importButton_);

    cancelButton_ = new QPushButton(tr("Cancel"));
    connect(cancelButton_, &QPushButton::clicked,
            this, &ImportTradeDialog::onCancelClicked);
    buttonLayout->addWidget(cancelButton_);

    mainLayout->addLayout(buttonLayout);
}

void ImportTradeDialog::populateTradeTable() {
    BOOST_LOG_SEV(lg(), debug) << "Populating trade table with "
                               << items_.size() << " trades";

    tradeTable_->setRowCount(static_cast<int>(items_.size()));

    int valid_count = 0;
    int invalid_count = 0;

    for (size_t i = 0; i < items_.size(); ++i) {
        const auto& item = items_[i];
        const auto& validation_error = validation_errors_[i];
        const bool is_valid = validation_error.empty();
        const int row = static_cast<int>(i);

        // Column 0: checkbox
        auto* checkBoxWidget = new QWidget();
        auto* checkBoxLayout = new QHBoxLayout(checkBoxWidget);
        checkBoxLayout->setContentsMargins(0, 0, 0, 0);
        checkBoxLayout->setAlignment(Qt::AlignCenter);

        auto* checkBox = new QCheckBox();
        checkBox->setProperty("row", row);
        connect(checkBox, &QCheckBox::toggled,
                this, [this](bool) { onTradeCheckChanged(); });

        if (!is_valid) {
            checkBox->setChecked(false);
            checkBox->setEnabled(false);
            checkBoxWidget->setToolTip(
                QString("Cannot import: %1")
                .arg(QString::fromStdString(validation_error)));
            invalid_count++;
        } else {
            checkBox->setChecked(true);
            valid_count++;
        }

        checkBoxLayout->addWidget(checkBox);
        tradeTable_->setCellWidget(row, 0, checkBoxWidget);

        // Columns 1–2: read-only text items
        const QString source_tooltip =
            QString("Source: %1")
            .arg(QString::fromStdString(item.source_file.generic_string()));

        auto makeItem = [&](const std::string& text) {
            auto* it = new QTableWidgetItem(QString::fromStdString(text));
            if (!is_valid) {
                it->setBackground(QBrush(QColor(255, 200, 200)));
                it->setToolTip(
                    QString("Source: %1\nValidation errors:\n%2")
                    .arg(QString::fromStdString(
                             item.source_file.generic_string()))
                    .arg(QString::fromStdString(validation_error)));
            } else {
                it->setToolTip(source_tooltip);
            }
            return it;
        };

        tradeTable_->setItem(row, 1, makeItem(item.trade.external_id));
        tradeTable_->setItem(row, 2, makeItem(item.trade.trade_type));

        // Column 3: per-row counterparty combo (populated after load)
        auto* cpCombo = new QComboBox();
        cpCombo->addItem(tr("-- None --"), QString());
        cpCombo->setProperty("ore_name",
            QString::fromStdString(item.ore_counterparty_name));
        if (!is_valid) cpCombo->setEnabled(false);
        tradeTable_->setCellWidget(row, 3, cpCombo);

        // Column 4: per-row editable netting set item
        auto* nsItem = new QTableWidgetItem(
            QString::fromStdString(item.trade.netting_set_id));
        if (is_valid) {
            nsItem->setFlags(nsItem->flags() | Qt::ItemIsEditable);
        } else {
            nsItem->setFlags(nsItem->flags() & ~Qt::ItemIsEditable);
            nsItem->setBackground(QBrush(QColor(255, 200, 200)));
        }
        tradeTable_->setItem(row, 4, nsItem);
    }

    BOOST_LOG_SEV(lg(), debug) << "Trade table populated: " << valid_count
                               << " valid, " << invalid_count << " invalid";
}

void ImportTradeDialog::populateCounterpartyCombos() {
    const auto count = counterparties_.size();
    BOOST_LOG_SEV(lg(), debug) << "Populating counterparty combos with "
                               << count << " counterparties";
    if (count == 0) {
        counterpartyStatusLabel_->setText(tr("(no counterparties found)"));
    } else {
        counterpartyStatusLabel_->setText(
            tr("(%1 loaded)").arg(static_cast<int>(count)));
    }

    // Repopulate the default counterparty combo
    const QString prevDefaultData =
        defaultCounterpartyCombo_->currentData().toString();
    defaultCounterpartyCombo_->clear();
    defaultCounterpartyCombo_->addItem(tr("-- None --"), QString());
    for (const auto& cp : counterparties_) {
        defaultCounterpartyCombo_->addItem(
            QString::fromStdString(cp.full_name),
            QString::fromStdString(boost::uuids::to_string(cp.id)));
    }
    if (!prevDefaultData.isEmpty()) {
        for (int j = 1; j < defaultCounterpartyCombo_->count(); ++j) {
            if (defaultCounterpartyCombo_->itemData(j).toString() ==
                    prevDefaultData) {
                defaultCounterpartyCombo_->setCurrentIndex(j);
                break;
            }
        }
    }

    // Repopulate per-row combos and try auto-match by ORE name
    for (int row = 0; row < tradeTable_->rowCount(); ++row) {
        auto* cpWidget = tradeTable_->cellWidget(row, 3);
        auto* cpCombo = cpWidget ? qobject_cast<QComboBox*>(cpWidget) : nullptr;
        if (!cpCombo || !cpCombo->isEnabled()) continue;

        const QString oreName = cpCombo->property("ore_name").toString();
        cpCombo->clear();
        cpCombo->addItem(tr("-- None --"), QString());
        for (const auto& cp : counterparties_) {
            cpCombo->addItem(
                QString::fromStdString(cp.full_name),
                QString::fromStdString(boost::uuids::to_string(cp.id)));
        }

        // Auto-match by ORE counterparty name (case-insensitive)
        if (!oreName.isEmpty()) {
            const QString oreLower = oreName.toLower();
            for (int j = 1; j < cpCombo->count(); ++j) {
                if (cpCombo->itemText(j).toLower() == oreLower) {
                    cpCombo->setCurrentIndex(j);
                    break;
                }
            }
        }
    }
}

void ImportTradeDialog::loadCounterparties() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Skipping counterparty load: not connected";
        counterpartyStatusLabel_->setText(tr("(not connected)"));
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading counterparties for mapping";

    struct Result {
        bool success = false;
        std::vector<refdata::domain::counterparty> counterparties;
    };

    QPointer<ImportTradeDialog> self = this;

    QFuture<Result> future = QtConcurrent::run([self]() -> Result {
        if (!self || !self->clientManager_)
            return {};

        refdata::messaging::get_counterparties_request request;
        request.offset = 0;
        request.limit = 1000; // server maximum
        auto payload = request.serialize();

        comms::messaging::frame req_frame(
            comms::messaging::message_type::get_counterparties_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(
            std::move(req_frame));
        if (!response_result) return {};

        auto decompressed = response_result->decompressed_payload();
        if (!decompressed) return {};

        auto response = refdata::messaging::get_counterparties_response::
            deserialize(*decompressed);
        if (!response) return {};

        return {true, std::move(response->counterparties)};
    });

    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, [this, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to load counterparties";
            counterpartyStatusLabel_->setText(tr("(failed to load)"));
            return;
        }

        counterparties_ = std::move(result.counterparties);
        BOOST_LOG_SEV(lg(), debug) << "Loaded " << counterparties_.size()
                                   << " counterparties";
        populateCounterpartyCombos();
    });

    watcher->setFuture(future);
}

void ImportTradeDialog::updateSelectionCount() {
    int selectedCount = 0;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked())
                selectedCount++;
        }
    }

    selectionCountLabel_->setText(
        QString("(%1 of %2 trades selected)")
        .arg(selectedCount)
        .arg(items_.size()));

    updateImportButtonState();
}

void ImportTradeDialog::updateImportButtonState() {
    int selectedCount = 0;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked())
                selectedCount++;
        }
    }
    importButton_->setEnabled(selectedCount > 0 && !importInProgress_);
}

void ImportTradeDialog::onSelectAllChanged(Qt::CheckState state) {
    BOOST_LOG_SEV(lg(), debug) << "Select all changed: "
                               << static_cast<int>(state);

    const bool checked = (state == Qt::Checked);
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isEnabled())
                checkBox->setChecked(checked);
        }
    }
    updateSelectionCount();
}

void ImportTradeDialog::onTradeCheckChanged() {
    updateSelectionCount();

    int checkedCount = 0;
    int enabledCount = 0;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isEnabled()) {
                enabledCount++;
                if (checkBox->isChecked())
                    checkedCount++;
            }
        }
    }

    selectAllCheckbox_->blockSignals(true);
    if (checkedCount == 0) {
        selectAllCheckbox_->setCheckState(Qt::Unchecked);
    } else if (checkedCount == enabledCount) {
        selectAllCheckbox_->setCheckState(Qt::Checked);
    } else {
        selectAllCheckbox_->setCheckState(Qt::PartiallyChecked);
    }
    selectAllCheckbox_->blockSignals(false);
}

void ImportTradeDialog::onApplyNettingSetToAll() {
    const QString value = defaultNettingSetEdit_->text();
    BOOST_LOG_SEV(lg(), debug) << "Applying netting set to all rows: "
                               << value.toStdString();
    for (int row = 0; row < tradeTable_->rowCount(); ++row) {
        auto* nsItem = tradeTable_->item(row, 4);
        if (nsItem && (nsItem->flags() & Qt::ItemIsEditable))
            nsItem->setText(value);
    }
}

void ImportTradeDialog::onApplyCounterpartyToAll() {
    const QString defaultData =
        defaultCounterpartyCombo_->currentData().toString();
    BOOST_LOG_SEV(lg(), debug) << "Applying counterparty to all rows: "
                               << defaultData.toStdString();
    for (int row = 0; row < tradeTable_->rowCount(); ++row) {
        auto* cpWidget = tradeTable_->cellWidget(row, 3);
        auto* cpCombo = cpWidget ? qobject_cast<QComboBox*>(cpWidget) : nullptr;
        if (!cpCombo || !cpCombo->isEnabled()) continue;
        for (int j = 0; j < cpCombo->count(); ++j) {
            if (cpCombo->itemData(j).toString() == defaultData) {
                cpCombo->setCurrentIndex(j);
                break;
            }
        }
    }
}

void ImportTradeDialog::onImportClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Import button clicked";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Import cancelled: client disconnected";
        MessageBoxHelper::warning(this, tr("Disconnected"),
            tr("Cannot import trades while disconnected."));
        return;
    }

    importInProgress_ = true;
    importButton_->setEnabled(false);
    selectAllCheckbox_->setEnabled(false);
    tradeTable_->setEnabled(false);

    progressBar_->setVisible(true);
    statusLabel_->setVisible(true);

    // Read global defaults from UI (on main thread before spawning worker)
    const std::string tradeDate =
        tradeDateEdit_->date().toString("yyyy-MM-dd").toStdString();
    const std::string effectiveDate =
        effectiveDateEdit_->date().toString("yyyy-MM-dd").toStdString();
    const std::string terminationDate =
        terminationDateEdit_->date().toString("yyyy-MM-dd").toStdString();
    const std::string lifecycleEvent =
        lifecycleEventCombo_->currentText().toStdString();
    const std::string executionTimestamp =
        QDateTime::currentDateTimeUtc()
        .toString("yyyy-MM-ddThh:mm:ss").toStdString();

    // Collect selected trades with per-row widget values
    struct TradeToImport {
        trading::domain::trade trade;
    };

    std::vector<TradeToImport> selected;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (!cellWidget) continue;
        auto* checkBox = cellWidget->findChild<QCheckBox*>();
        if (!checkBox || !checkBox->isChecked()) continue;

        const auto& item = items_[static_cast<size_t>(i)];

        TradeToImport tti;
        tti.trade = item.trade;

        tti.trade.id = boost::uuids::random_generator()();
        tti.trade.book_id = book_.id;
        tti.trade.portfolio_id = book_.parent_portfolio_id;
        tti.trade.party_id = book_.party_id;
        tti.trade.modified_by = username_.toStdString();

        // Apply global date/lifecycle defaults
        tti.trade.trade_date = tradeDate;
        tti.trade.effective_date = effectiveDate;
        tti.trade.termination_date = terminationDate;
        tti.trade.lifecycle_event = lifecycleEvent;
        tti.trade.execution_timestamp = executionTimestamp;

        // Per-row netting set
        auto* nsItem = tradeTable_->item(i, 4);
        if (nsItem)
            tti.trade.netting_set_id = nsItem->text().toStdString();

        // Per-row counterparty
        auto* cpWidget = tradeTable_->cellWidget(i, 3);
        auto* cpCombo = cpWidget ? qobject_cast<QComboBox*>(cpWidget) : nullptr;
        if (cpCombo && cpCombo->currentIndex() > 0) {
            const QString uuid_str = cpCombo->currentData().toString();
            if (!uuid_str.isEmpty()) {
                try {
                    tti.trade.counterparty_id =
                        boost::lexical_cast<boost::uuids::uuid>(
                            uuid_str.toStdString());
                } catch (...) {
                    tti.trade.counterparty_id = std::nullopt;
                }
            }
        } else {
            tti.trade.counterparty_id = std::nullopt;
        }

        selected.push_back(std::move(tti));
    }

    const int total = static_cast<int>(selected.size());
    BOOST_LOG_SEV(lg(), info) << "Starting import of " << total << " trades";

    progressBar_->setRange(0, total);
    progressBar_->setValue(0);
    statusLabel_->setText(tr("Starting import..."));

    QPointer<ImportTradeDialog> self = this;

    QFuture<std::pair<int, int>> future =
        QtConcurrent::run([self, selected, total]() -> std::pair<int, int> {
            using namespace ores::trading::messaging;

            int success_count = 0;
            int current = 0;

            for (const auto& tti : selected) {
                if (!self || self->cancelRequested_.load()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Import cancelled by user at trade "
                        << current << " of " << total;
                    break;
                }

                current++;

                QMetaObject::invokeMethod(self,
                    [self, current, total, ext_id = tti.trade.external_id]() {
                        if (!self) return;
                        self->progressBar_->setValue(current);
                        self->statusLabel_->setText(
                            QString("Importing %1 (%2 of %3)...")
                            .arg(QString::fromStdString(ext_id))
                            .arg(current)
                            .arg(total));
                    }, Qt::QueuedConnection);

                if (!self) break;

                try {
                    save_trade_request request{tti.trade};
                    auto payload = request.serialize();
                    comms::messaging::frame request_frame(
                        comms::messaging::message_type::save_trade_request,
                        0, std::move(payload));

                    auto response_result = self->clientManager_->sendRequest(
                        std::move(request_frame));

                    if (!response_result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to import trade: "
                            << tti.trade.external_id;
                        continue;
                    }

                    auto payload_result =
                        response_result->decompressed_payload();
                    if (!payload_result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to decompress response for: "
                            << tti.trade.external_id;
                        continue;
                    }

                    auto response = save_trade_response::
                        deserialize(*payload_result);

                    if (response && response->success) {
                        success_count++;
                        BOOST_LOG_SEV(lg(), debug)
                            << "Successfully imported trade: "
                            << tti.trade.external_id;
                    } else {
                        const std::string msg = response
                            ? response->message : "Unknown error";
                        BOOST_LOG_SEV(lg(), warn)
                            << "Server rejected trade: "
                            << tti.trade.external_id << " - " << msg;
                    }
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Error importing trade "
                        << tti.trade.external_id << ": " << e.what();
                }
            }

            return {success_count, total};
        });

    auto* watcher = new QFutureWatcher<std::pair<int, int>>(this);
    connect(watcher, &QFutureWatcher<std::pair<int, int>>::finished,
            this, [this, watcher]() {
        auto result = watcher->result();
        const int success_count = result.first;
        const int total_count = result.second;

        progressBar_->setVisible(false);
        statusLabel_->setVisible(false);
        importInProgress_ = false;

        if (cancelRequested_.load()) {
            BOOST_LOG_SEV(lg(), info)
                << "Import cancelled after importing " << success_count
                << " of " << total_count << " trades";
            emit importCancelled();
            reject();
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Import completed: " << success_count
                << " of " << total_count << " trades imported successfully";
            emit importCompleted(success_count, total_count);
            accept();
        }

        watcher->deleteLater();
    });

    watcher->setFuture(future);
}

void ImportTradeDialog::onCancelClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Cancel button clicked";

    if (importInProgress_) {
        cancelRequested_.store(true);
        BOOST_LOG_SEV(lg(), info) << "Cancellation requested";
        statusLabel_->setText(tr("Cancelling import..."));
        cancelButton_->setEnabled(false);
    } else {
        reject();
    }
}

}
