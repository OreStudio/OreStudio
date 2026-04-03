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
#include "ores.qt/TradeDetailDialog.hpp"

#include <QComboBox>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ui_TradeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ---------------------------------------------------------------------------
// FX option trade type detection
// ---------------------------------------------------------------------------

static bool isFxOptionType(const QString& tradeTypeCode) {
    return tradeTypeCode.contains("Option", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Collar", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Barrier", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Digital", Qt::CaseInsensitive);
}

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

TradeDetailDialog::TradeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::TradeDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

TradeDetailDialog::~TradeDetailDialog() {
    delete ui_;
}

QTabWidget*       TradeDetailDialog::tabWidget()        const { return ui_->tabWidget; }
QWidget*          TradeDetailDialog::provenanceTab()    const { return ui_->provenanceTab; }
ProvenanceWidget* TradeDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

void TradeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);
    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // Hide instrument tabs and instrument provenance section until an
    // instrument loads.
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxOptionsTab), false);
    ui_->instrumentProvenanceGroup->setVisible(false);
}

void TradeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &TradeDetailDialog::onCloseClicked);

    // Trade fields
    connect(ui_->bookCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &TradeDetailDialog::onFieldChanged);
    connect(ui_->counterpartyCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &TradeDetailDialog::onFieldChanged);
    connect(ui_->externalIdEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onCodeChanged);
    connect(ui_->tradeTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->lifecycleEventEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->nettingSetIdEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->tradeDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->effectiveDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->terminationDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);
    connect(ui_->executionTimestampEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFieldChanged);

    // FX instrument fields
    connect(ui_->fxTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onFxTradeTypeChanged);
    connect(ui_->fxBoughtCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxSoldCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxValueDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxSettlementEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxOptionTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxExpiryDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxBoughtAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxSoldAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->fxStrikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
}

// ---------------------------------------------------------------------------
// Client manager and reference data loading
// ---------------------------------------------------------------------------

void TradeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    loadBooks();
    loadCounterparties();
}

void TradeDetailDialog::loadBooks() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    struct BooksResult {
        bool success;
        std::vector<refdata::domain::book> books;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<BooksResult>(self);
    connect(watcher, &QFutureWatcher<BooksResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self || !result.success) return;

        self->books_ = std::move(result.books);
        self->ui_->bookCombo->blockSignals(true);
        self->ui_->bookCombo->clear();
        for (const auto& book : self->books_) {
            QString name = QString::fromStdString(book.name);
            QString id = QString::fromStdString(boost::uuids::to_string(book.id));
            self->ui_->bookCombo->addItem(name, id);
        }
        self->ui_->bookCombo->blockSignals(false);
        self->selectCurrentBook();
    });

    watcher->setFuture(QtConcurrent::run([self]() -> BooksResult {
        if (!self || !self->clientManager_) return {false, {}};
        refdata::messaging::get_books_request req;
        req.offset = 0;
        req.limit = 1000;
        auto r = self->clientManager_->process_authenticated_request(std::move(req));
        if (!r) return {false, {}};
        return {true, std::move(r->books)};
    }));
}

void TradeDetailDialog::selectCurrentBook() {
    if (trade_.book_id.is_nil()) return;
    const QString target =
        QString::fromStdString(boost::uuids::to_string(trade_.book_id));
    for (int i = 0; i < ui_->bookCombo->count(); ++i) {
        if (ui_->bookCombo->itemData(i).toString() == target) {
            ui_->bookCombo->blockSignals(true);
            ui_->bookCombo->setCurrentIndex(i);
            ui_->bookCombo->blockSignals(false);
            return;
        }
    }
}

void TradeDetailDialog::loadCounterparties() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    struct CpResult {
        bool success;
        std::vector<refdata::domain::counterparty> counterparties;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CpResult>(self);
    connect(watcher, &QFutureWatcher<CpResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self || !result.success) return;

        self->counterparties_ = std::move(result.counterparties);
        self->ui_->counterpartyCombo->blockSignals(true);
        self->ui_->counterpartyCombo->clear();
        self->ui_->counterpartyCombo->addItem(tr("-- None --"), QString());
        for (const auto& cp : self->counterparties_) {
            QString name = QString::fromStdString(cp.full_name);
            QString id = QString::fromStdString(boost::uuids::to_string(cp.id));
            self->ui_->counterpartyCombo->addItem(name, id);
        }
        self->ui_->counterpartyCombo->blockSignals(false);
        self->selectCurrentCounterparty();
    });

    watcher->setFuture(QtConcurrent::run([self]() -> CpResult {
        if (!self || !self->clientManager_) return {false, {}};
        refdata::messaging::get_counterparties_request req;
        req.offset = 0;
        req.limit = 1000;
        auto r = self->clientManager_->process_authenticated_request(std::move(req));
        if (!r) return {false, {}};
        return {true, std::move(r->counterparties)};
    }));
}

void TradeDetailDialog::selectCurrentCounterparty() {
    if (!trade_.counterparty_id.has_value()) {
        ui_->counterpartyCombo->blockSignals(true);
        ui_->counterpartyCombo->setCurrentIndex(0);
        ui_->counterpartyCombo->blockSignals(false);
        return;
    }
    const QString target =
        QString::fromStdString(boost::uuids::to_string(*trade_.counterparty_id));
    for (int i = 0; i < ui_->counterpartyCombo->count(); ++i) {
        if (ui_->counterpartyCombo->itemData(i).toString() == target) {
            ui_->counterpartyCombo->blockSignals(true);
            ui_->counterpartyCombo->setCurrentIndex(i);
            ui_->counterpartyCombo->blockSignals(false);
            return;
        }
    }
}

// ---------------------------------------------------------------------------
// Public setters
// ---------------------------------------------------------------------------

void TradeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TradeDetailDialog::setTrade(const trading::domain::trade& trade) {
    trade_ = trade;
    updateUiFromTrade();
    selectCurrentBook();
    selectCurrentCounterparty();

    if (trade_.product_type == "fx" && trade_.instrument_id.has_value())
        loadFxInstrument();
}

void TradeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->bookCombo->setEnabled(createMode);
    ui_->counterpartyCombo->setEnabled(createMode);
    ui_->externalIdEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode)
        trade_.id = boost::uuids::random_generator()();

    // Instrument tabs never shown in create mode
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxEconomicsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxOptionsTab), false);
    ui_->instrumentProvenanceGroup->setVisible(false);

    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    instrumentHasChanges_ = false;
    updateSaveButtonState();
}

void TradeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->bookCombo->setEnabled(!readOnly);
    ui_->counterpartyCombo->setEnabled(!readOnly);
    ui_->externalIdEdit->setReadOnly(true);
    ui_->tradeTypeEdit->setReadOnly(readOnly);
    ui_->lifecycleEventEdit->setReadOnly(readOnly);
    ui_->nettingSetIdEdit->setReadOnly(readOnly);
    ui_->tradeDateEdit->setReadOnly(readOnly);
    ui_->effectiveDateEdit->setReadOnly(readOnly);
    ui_->terminationDateEdit->setReadOnly(readOnly);
    ui_->executionTimestampEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    setFxReadOnly(readOnly);
}

// ---------------------------------------------------------------------------
// Trade UI helpers
// ---------------------------------------------------------------------------

void TradeDetailDialog::updateUiFromTrade() {
    ui_->externalIdEdit->setText(QString::fromStdString(trade_.external_id));
    ui_->tradeTypeEdit->setText(QString::fromStdString(trade_.trade_type));
    ui_->lifecycleEventEdit->setText(
        QString::fromStdString(trade_.activity_type_code));
    ui_->nettingSetIdEdit->setText(
        QString::fromStdString(trade_.netting_set_id));
    ui_->tradeDateEdit->setText(QString::fromStdString(trade_.trade_date));
    ui_->effectiveDateEdit->setText(
        QString::fromStdString(trade_.effective_date));
    ui_->terminationDateEdit->setText(
        QString::fromStdString(trade_.termination_date));
    ui_->executionTimestampEdit->setText(
        QString::fromStdString(trade_.execution_timestamp));

    populateProvenance(trade_.version, trade_.modified_by, trade_.performed_by,
                       trade_.recorded_at, trade_.change_reason_code,
                       trade_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TradeDetailDialog::updateTradeFromUi() {
    const int bookIdx = ui_->bookCombo->currentIndex();
    if (bookIdx >= 0 && bookIdx < static_cast<int>(books_.size())) {
        const auto& book = books_[static_cast<std::size_t>(bookIdx)];
        trade_.book_id = book.id;
        trade_.portfolio_id = book.parent_portfolio_id;
    }
    const int cpIdx = ui_->counterpartyCombo->currentIndex();
    if (cpIdx > 0) {
        const auto& cp = counterparties_[static_cast<std::size_t>(cpIdx - 1)];
        trade_.counterparty_id = cp.id;
    } else {
        trade_.counterparty_id = std::nullopt;
    }
    if (createMode_)
        trade_.external_id = ui_->externalIdEdit->text().trimmed().toStdString();
    trade_.trade_type =
        ui_->tradeTypeEdit->text().trimmed().toStdString();
    trade_.activity_type_code =
        ui_->lifecycleEventEdit->text().trimmed().toStdString();
    trade_.netting_set_id =
        ui_->nettingSetIdEdit->text().trimmed().toStdString();
    trade_.trade_date =
        ui_->tradeDateEdit->text().trimmed().toStdString();
    trade_.effective_date =
        ui_->effectiveDateEdit->text().trimmed().toStdString();
    trade_.termination_date =
        ui_->terminationDateEdit->text().trimmed().toStdString();
    trade_.execution_timestamp =
        ui_->executionTimestampEdit->text().trimmed().toStdString();
    trade_.modified_by = username_;
    trade_.performed_by = username_;
}

// ---------------------------------------------------------------------------
// FX instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadFxInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const std::string family = trade_.product_type;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct FxResult {
        bool success;
        std::string message;
        trading::domain::fx_instrument instrument;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<FxResult>(self);
    connect(watcher, &QFutureWatcher<FxResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load FX instrument: " << result.message;
            return;
        }

        self->fxInstrument_ = std::move(result.instrument);
        self->instrumentLoaded_ = true;
        self->populateFxInstrument();
    });

    watcher->setFuture(QtConcurrent::run([self, family, id]() -> FxResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed", {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}};
        if (!r->success)
            return {false, r->message, {}};

        const auto* fx =
            std::get_if<trading::domain::fx_instrument>(&r->instrument);
        if (!fx)
            return {false, "Unexpected instrument type in response", {}};

        return {true, {}, *fx};
    }));
}

void TradeDetailDialog::populateFxInstrument() {
    // Block signals while setting values so instrumentHasChanges_ stays false.
    const auto block = [this](bool b) {
        ui_->fxTradeTypeCodeEdit->blockSignals(b);
        ui_->fxBoughtCurrencyEdit->blockSignals(b);
        ui_->fxBoughtAmountSpinBox->blockSignals(b);
        ui_->fxSoldCurrencyEdit->blockSignals(b);
        ui_->fxSoldAmountSpinBox->blockSignals(b);
        ui_->fxValueDateEdit->blockSignals(b);
        ui_->fxSettlementEdit->blockSignals(b);
        ui_->fxOptionTypeEdit->blockSignals(b);
        ui_->fxStrikePriceSpinBox->blockSignals(b);
        ui_->fxExpiryDateEdit->blockSignals(b);
        ui_->fxDescriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->fxTradeTypeCodeEdit->setText(
        QString::fromStdString(fxInstrument_.trade_type_code));
    ui_->fxBoughtCurrencyEdit->setText(
        QString::fromStdString(fxInstrument_.bought_currency));
    ui_->fxBoughtAmountSpinBox->setValue(fxInstrument_.bought_amount);
    ui_->fxSoldCurrencyEdit->setText(
        QString::fromStdString(fxInstrument_.sold_currency));
    ui_->fxSoldAmountSpinBox->setValue(fxInstrument_.sold_amount);
    ui_->fxValueDateEdit->setText(
        QString::fromStdString(fxInstrument_.value_date.value_or("")));
    ui_->fxSettlementEdit->setText(
        QString::fromStdString(fxInstrument_.settlement));
    ui_->fxOptionTypeEdit->setText(
        QString::fromStdString(fxInstrument_.option_type));
    ui_->fxStrikePriceSpinBox->setValue(fxInstrument_.strike_price);
    ui_->fxExpiryDateEdit->setText(
        QString::fromStdString(fxInstrument_.expiry_date));
    ui_->fxDescriptionEdit->setPlainText(
        QString::fromStdString(fxInstrument_.description));
    block(false);

    // Populate instrument provenance section
    ui_->instrumentProvenanceWidget->populate(
        fxInstrument_.version,
        fxInstrument_.modified_by,
        fxInstrument_.performed_by,
        fxInstrument_.recorded_at,
        fxInstrument_.change_reason_code,
        fxInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateFxTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateFxInstrumentFromUi() {
    fxInstrument_.trade_type_code =
        ui_->fxTradeTypeCodeEdit->text().trimmed().toStdString();
    fxInstrument_.bought_currency =
        ui_->fxBoughtCurrencyEdit->text().trimmed().toStdString();
    fxInstrument_.bought_amount = ui_->fxBoughtAmountSpinBox->value();
    fxInstrument_.sold_currency =
        ui_->fxSoldCurrencyEdit->text().trimmed().toStdString();
    fxInstrument_.sold_amount = ui_->fxSoldAmountSpinBox->value();
    {
        const auto vd = ui_->fxValueDateEdit->text().trimmed().toStdString();
        fxInstrument_.value_date = vd.empty() ? std::nullopt : std::optional(vd);
    }
    fxInstrument_.settlement =
        ui_->fxSettlementEdit->text().trimmed().toStdString();
    fxInstrument_.option_type =
        ui_->fxOptionTypeEdit->text().trimmed().toStdString();
    fxInstrument_.strike_price = ui_->fxStrikePriceSpinBox->value();
    fxInstrument_.expiry_date =
        ui_->fxExpiryDateEdit->text().trimmed().toStdString();
    fxInstrument_.description =
        ui_->fxDescriptionEdit->toPlainText().trimmed().toStdString();
    fxInstrument_.modified_by = username_;
    fxInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateFxTabVisibility() {
    const QString tradeType = ui_->fxTradeTypeCodeEdit->text().trimmed();
    const bool showEconomics =
        instrumentLoaded_ && !tradeType.isEmpty();
    const bool showOptions =
        instrumentLoaded_ && isFxOptionType(tradeType);

    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxEconomicsTab), showEconomics);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->fxOptionsTab), showOptions);
}

void TradeDetailDialog::setFxReadOnly(bool readOnly) {
    ui_->fxTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->fxBoughtCurrencyEdit->setReadOnly(readOnly);
    ui_->fxBoughtAmountSpinBox->setReadOnly(readOnly);
    ui_->fxSoldCurrencyEdit->setReadOnly(readOnly);
    ui_->fxSoldAmountSpinBox->setReadOnly(readOnly);
    ui_->fxValueDateEdit->setReadOnly(readOnly);
    ui_->fxSettlementEdit->setReadOnly(readOnly);
    ui_->fxOptionTypeEdit->setReadOnly(readOnly);
    ui_->fxStrikePriceSpinBox->setReadOnly(readOnly);
    ui_->fxExpiryDateEdit->setReadOnly(readOnly);
    ui_->fxDescriptionEdit->setReadOnly(readOnly);
}

// ---------------------------------------------------------------------------
// Change tracking
// ---------------------------------------------------------------------------

void TradeDetailDialog::onCodeChanged(const QString&) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onInstrumentFieldChanged() {
    if (!instrumentLoaded_) return;
    instrumentHasChanges_ = true;
    updateSaveButtonState();
}

void TradeDetailDialog::onFxTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateFxTabVisibility();
        updateSaveButtonState();
    }
}

void TradeDetailDialog::updateSaveButtonState() {
    const bool canSave =
        (hasChanges_ || instrumentHasChanges_) && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TradeDetailDialog::validateInput() {
    if (ui_->bookCombo->currentIndex() < 0) return false;
    return !ui_->tradeTypeEdit->text().trimmed().isEmpty() &&
           !ui_->lifecycleEventEdit->text().trimmed().isEmpty() &&
           !ui_->tradeDateEdit->text().trimmed().isEmpty() &&
           !ui_->effectiveDateEdit->text().trimmed().isEmpty() &&
           !ui_->terminationDateEdit->text().trimmed().isEmpty();
}

// ---------------------------------------------------------------------------
// Save
// ---------------------------------------------------------------------------

void TradeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save while disconnected from server.");
        return;
    }
    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateTradeFromUi();
    if (instrumentLoaded_)
        updateFxInstrumentFromUi();

    const auto crOpType = createMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType,
        hasChanges_ || instrumentHasChanges_,
        createMode_ ? "system" : "common");
    if (!crSel) return;

    trade_.change_reason_code = crSel->reason_code;
    trade_.change_commentary  = crSel->commentary;

    if (instrumentLoaded_) {
        // Instrument change reason matches the trade change reason so that
        // both records carry the same amendment rationale.
        fxInstrument_.change_reason_code = crSel->reason_code;
        fxInstrument_.change_commentary  = crSel->commentary;
    }

    if (instrumentHasChanges_ && instrumentLoaded_) {
        // Instrument changed: save instrument first, then always save trade
        // so that the trade version bumps and downstream consumers are notified.
        BOOST_LOG_SEV(lg(), info) << "Saving FX instrument then trade: "
                                  << trade_.external_id;
        saveFxThenTrade(trade_, fxInstrument_);
    } else {
        BOOST_LOG_SEV(lg(), info) << "Saving trade only: " << trade_.external_id;
        saveTrade(trade_);
    }
}

void TradeDetailDialog::saveFxThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::fx_instrument& instrument) {

    struct FxSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<FxSaveResult>(self);
    connect(watcher, &QFutureWatcher<FxSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "FX instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save FX instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "FX instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    watcher->setFuture(QtConcurrent::run(
        [self, instrument]() -> FxSaveResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};
        trading::messaging::save_fx_instrument_request req;
        req.data = instrument;
        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

void TradeDetailDialog::saveTrade(const trading::domain::trade& trade) {
    struct TradeResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<TradeResult>(self);
    connect(watcher, &QFutureWatcher<TradeResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Trade saved successfully";
            QString code = QString::fromStdString(self->trade_.external_id);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->tradeSaved(code);
            self->notifySaveSuccess(tr("Trade '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Trade save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    watcher->setFuture(QtConcurrent::run([self, trade]() -> TradeResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};
        trading::messaging::save_trade_request req;
        req.trades.push_back(trade);
        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

// ---------------------------------------------------------------------------
// Delete
// ---------------------------------------------------------------------------

void TradeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete trade while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(trade_.external_id);
    auto reply = MessageBoxHelper::question(this, "Delete Trade",
        QString("Are you sure you want to delete trade '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    const auto crSel = promptChangeReason(
        ChangeReasonDialog::OperationType::Delete, true, "common");
    if (!crSel) return;

    BOOST_LOG_SEV(lg(), info) << "Deleting trade: " << trade_.external_id;

    struct DeleteResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (result.success) {
            emit self->statusMessage(tr("Trade '%1' deleted").arg(code));
            emit self->tradeDeleted(code);
            self->requestClose();
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    watcher->setFuture(QtConcurrent::run([self, id = trade_.id]() -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Dialog closed"};
        trading::messaging::delete_trade_request req;
        req.ids.push_back(boost::uuids::to_string(id));
        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
