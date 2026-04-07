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
#include "ores.qt/CompositeLegsWidget.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// Local alias to keep dispatch sites concise without conflicting with the
// trade.product_type member name.
using PT = ores::trading::domain::product_type;

// ---------------------------------------------------------------------------
// Trade type detection helpers
// ---------------------------------------------------------------------------

static bool isCommodityExtensionType(const QString& tradeTypeCode) {
    return tradeTypeCode.contains("Option", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Spread", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Swaption", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Accumulator", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Strip", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Average", Qt::CaseInsensitive) ||
           tradeTypeCode.contains("Variance", Qt::CaseInsensitive);
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

    // Build a stack page per registered IInstrumentForm. Each form's
    // signals are wired to the dialog's shared instrument provenance
    // widget and dirty-tracking flag.
    register_default_forms(instrumentFormRegistry_);
    for (const auto pt : instrumentFormRegistry_.registeredTypes()) {
        IInstrumentForm* form =
            instrumentFormRegistry_.createForm(pt, ui_->instrumentStack);
        ui_->instrumentStack->addWidget(form);

        connect(form, &IInstrumentForm::changed, this,
                &TradeDetailDialog::onInstrumentFieldChanged);
        connect(form, &IInstrumentForm::instrumentLoaded, this, [this]() {
            instrumentLoaded_ = true;
            ui_->instrumentProvenanceGroup->setVisible(true);
            updateSaveButtonState();
        });
        connect(form, &IInstrumentForm::loadFailed, this,
                [](const QString& err) {
            BOOST_LOG_SEV(lg(), warn)
                << "Instrument load failed: " << err.toStdString();
        });
        connect(form, &IInstrumentForm::provenanceChanged, this,
                [this](const InstrumentProvenance& p) {
            ui_->instrumentProvenanceWidget->populate(
                p.version, p.modified_by, p.performed_by,
                p.recorded_at, p.change_reason_code, p.change_commentary);
            ui_->instrumentProvenanceGroup->setVisible(true);
        });
    }
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

    // Hide all instrument tabs and instrument provenance section until an
    // instrument loads.
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->instrumentTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->commodityCoreTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->commodityExtensionsTab), false);
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


    // Commodity instrument fields
    connect(ui_->commodityTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onCommodityTradeTypeChanged);
    connect(ui_->commodityCommodityCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityCurrencyEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityUnitEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityStartDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityMaturityDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityOptionTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityExerciseTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityBarrierTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityAverageTypeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityAveragingStartDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityAveragingEndDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commoditySpreadCommodityCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityStripFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityDayCountCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityPaymentFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commoditySwaptionExpiryDateEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityBasketJsonEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityQuantitySpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityFixedPriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityStrikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityLowerBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityUpperBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commoditySpreadAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityVarianceStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityAccumulationAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->commodityKnockOutBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &TradeDetailDialog::onInstrumentFieldChanged);

    // Composite instrument fields
    connect(ui_->compositeTradeTypeCombo, &QComboBox::currentTextChanged, this,
            &TradeDetailDialog::onCompositeTradeTypeChanged);
    connect(ui_->compositeDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->compositeLegsWidget, &CompositeLegsWidget::legsChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);

    // Scripted instrument fields
    connect(ui_->scriptTradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onScriptTradeTypeChanged);
    connect(ui_->scriptNameEdit, &QLineEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->scriptDescriptionEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->scriptBodyEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->scriptEventsJsonEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->scriptUnderlyingsJsonEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);
    connect(ui_->scriptParametersJsonEdit, &QPlainTextEdit::textChanged, this,
            &TradeDetailDialog::onInstrumentFieldChanged);

    // FX instrument fields are owned by FxInstrumentForm; signal wiring
    // happens in the dialog constructor when the form is added to the stack.
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

    // If this product family has been migrated to an IInstrumentForm, route
    // to the new instrumentTab + stack widget; otherwise fall through to the
    // legacy per-family load functions still defined in this file.
    if (instrumentFormRegistry_.contains(trade_.product_type)) {
        activeForm_ = nullptr;
        for (int i = 0; i < ui_->instrumentStack->count(); ++i) {
            auto* form = qobject_cast<IInstrumentForm*>(
                ui_->instrumentStack->widget(i));
            if (!form) continue;
            // The stack pages are added in registry order; pick the page
            // whose registered family matches the current trade.
            const auto types = instrumentFormRegistry_.registeredTypes();
            if (i < static_cast<int>(types.size()) &&
                types[static_cast<std::size_t>(i)] == trade_.product_type) {
                activeForm_ = form;
                ui_->instrumentStack->setCurrentWidget(form);
                break;
            }
        }
        ui_->tabWidget->setTabVisible(
            ui_->tabWidget->indexOf(ui_->instrumentTab), true);
        if (activeForm_) {
            activeForm_->setClientManager(clientManager_);
            activeForm_->setUsername(username_);
            // TODO Phase 5: pass real has_options/has_extension flags from
            // the trade type lookup. For now keep the legacy default behaviour.
            activeForm_->setTradeType(
                QString::fromStdString(trade_.trade_type), true, false);
            if (trade_.instrument_id.has_value()) {
                activeForm_->loadInstrument(
                    boost::uuids::to_string(*trade_.instrument_id));
            } else {
                activeForm_->clear();
            }
        }
        return;
    }

    if (trade_.product_type == PT::commodity && trade_.instrument_id.has_value())
        loadCommodityInstrument();
    else if (trade_.product_type == PT::composite && trade_.instrument_id.has_value())
        loadCompositeInstrument();
    else if (trade_.product_type == PT::scripted && trade_.instrument_id.has_value())
        loadScriptedInstrument();
}

void TradeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->bookCombo->setEnabled(createMode);
    ui_->counterpartyCombo->setEnabled(createMode);
    ui_->externalIdEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode)
        trade_.id = boost::uuids::random_generator()();

    // Instrument tabs never shown in create mode (create-mode instrument
    // creation is enabled in Phase 6 once the registry covers all families).
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->instrumentTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->commodityCoreTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->commodityExtensionsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->compositeCoreTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->compositeLegsTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->scriptDefinitionTab), false);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->scriptBodyTab), false);
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
    if (activeForm_) activeForm_->setReadOnly(readOnly);
    setCommodityReadOnly(readOnly);
    setCompositeReadOnly(readOnly);
    setScriptedReadOnly(readOnly);
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

void TradeDetailDialog::onCommodityTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateCommodityTabVisibility();
        updateSaveButtonState();
    }
}

void TradeDetailDialog::onCompositeTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
        updateSaveButtonState();
    }
}

void TradeDetailDialog::onScriptTradeTypeChanged(const QString&) {
    if (instrumentLoaded_) {
        instrumentHasChanges_ = true;
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
    if (instrumentLoaded_) {
        if (activeForm_ &&
            instrumentFormRegistry_.contains(trade_.product_type)) {
            activeForm_->writeUiToInstrument();
        } else if (trade_.product_type == PT::commodity) {
            updateCommodityInstrumentFromUi();
        } else if (trade_.product_type == PT::composite) {
            updateCompositeInstrumentFromUi();
        } else if (trade_.product_type == PT::scripted) {
            updateScriptedInstrumentFromUi();
        }
    }

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
        // Instrument change reason matches the trade change reason.
        if (activeForm_ &&
            instrumentFormRegistry_.contains(trade_.product_type)) {
            activeForm_->setChangeReason(
                crSel->reason_code, crSel->commentary);
        } else if (trade_.product_type == PT::commodity) {
            commodityInstrument_.change_reason_code = crSel->reason_code;
            commodityInstrument_.change_commentary  = crSel->commentary;
        } else if (trade_.product_type == PT::composite) {
            compositeInstrument_.change_reason_code = crSel->reason_code;
            compositeInstrument_.change_commentary  = crSel->commentary;
        } else if (trade_.product_type == PT::scripted) {
            scriptedInstrument_.change_reason_code = crSel->reason_code;
            scriptedInstrument_.change_commentary  = crSel->commentary;
        }
    }

    if (instrumentHasChanges_ && instrumentLoaded_) {
        if (activeForm_ &&
            instrumentFormRegistry_.contains(trade_.product_type)) {
            BOOST_LOG_SEV(lg(), info)
                << "Saving instrument then trade via form: "
                << trade_.external_id;
            const auto saved_trade = trade_;
            activeForm_->saveInstrument(
                [this, saved_trade](const std::string& id) {
                    instrumentHasChanges_ = false;
                    auto t = saved_trade;
                    if (!id.empty())
                        t.instrument_id =
                            boost::uuids::string_generator()(id);
                    saveTrade(t);
                },
                [this](const QString& err) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Instrument save failed: " << err.toStdString();
                    emit errorMessage(err);
                    MessageBoxHelper::critical(this, "Save Failed",
                        tr("Failed to save instrument:\n%1").arg(err));
                });
            return;
        }
        if (trade_.product_type == PT::commodity) {
            BOOST_LOG_SEV(lg(), info) << "Saving commodity instrument then trade: "
                                      << trade_.external_id;
            saveCommodityThenTrade(trade_, commodityInstrument_);
        } else if (trade_.product_type == PT::composite) {
            BOOST_LOG_SEV(lg(), info) << "Saving composite instrument then trade: "
                                      << trade_.external_id;
            saveCompositeThenTrade(trade_, compositeInstrument_, compositeLegs_);
        } else if (trade_.product_type == PT::scripted) {
            BOOST_LOG_SEV(lg(), info) << "Saving scripted instrument then trade: "
                                      << trade_.external_id;
            saveScriptedThenTrade(trade_, scriptedInstrument_);
        }
    } else {
        BOOST_LOG_SEV(lg(), info) << "Saving trade only: " << trade_.external_id;
        saveTrade(trade_);
    }
}

// ---------------------------------------------------------------------------
// Commodity instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadCommodityInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const auto family = PT::commodity;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct CommodityResult {
        bool success;
        std::string message;
        trading::domain::commodity_instrument instrument;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CommodityResult>(self);
    connect(watcher, &QFutureWatcher<CommodityResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load commodity instrument: " << result.message;
            return;
        }

        self->commodityInstrument_ = std::move(result.instrument);
        self->instrumentLoaded_ = true;
        self->populateCommodityInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> CommodityResult {
        if (!cm)
            return {false, "Dialog closed", {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}};
        if (!r->success)
            return {false, r->message, {}};

        const auto* com =
            std::get_if<trading::domain::commodity_instrument>(&r->instrument);
        if (!com)
            return {false, "Unexpected instrument type in response", {}};

        return {true, {}, *com};
    }));
}

void TradeDetailDialog::populateCommodityInstrument() {
    const auto block = [this](bool b) {
        ui_->commodityTradeTypeCodeEdit->blockSignals(b);
        ui_->commodityCommodityCodeEdit->blockSignals(b);
        ui_->commodityCurrencyEdit->blockSignals(b);
        ui_->commodityQuantitySpinBox->blockSignals(b);
        ui_->commodityUnitEdit->blockSignals(b);
        ui_->commodityStartDateEdit->blockSignals(b);
        ui_->commodityMaturityDateEdit->blockSignals(b);
        ui_->commodityFixedPriceSpinBox->blockSignals(b);
        ui_->commodityOptionTypeEdit->blockSignals(b);
        ui_->commodityStrikePriceSpinBox->blockSignals(b);
        ui_->commodityExerciseTypeEdit->blockSignals(b);
        ui_->commodityAverageTypeEdit->blockSignals(b);
        ui_->commodityAveragingStartDateEdit->blockSignals(b);
        ui_->commodityAveragingEndDateEdit->blockSignals(b);
        ui_->commoditySpreadCommodityCodeEdit->blockSignals(b);
        ui_->commoditySpreadAmountSpinBox->blockSignals(b);
        ui_->commodityStripFrequencyCodeEdit->blockSignals(b);
        ui_->commodityVarianceStrikeSpinBox->blockSignals(b);
        ui_->commodityAccumulationAmountSpinBox->blockSignals(b);
        ui_->commodityKnockOutBarrierSpinBox->blockSignals(b);
        ui_->commodityBarrierTypeEdit->blockSignals(b);
        ui_->commodityLowerBarrierSpinBox->blockSignals(b);
        ui_->commodityUpperBarrierSpinBox->blockSignals(b);
        ui_->commodityBasketJsonEdit->blockSignals(b);
        ui_->commodityDayCountCodeEdit->blockSignals(b);
        ui_->commodityPaymentFrequencyCodeEdit->blockSignals(b);
        ui_->commoditySwaptionExpiryDateEdit->blockSignals(b);
        ui_->commodityDescriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->commodityTradeTypeCodeEdit->setText(
        QString::fromStdString(commodityInstrument_.trade_type_code));
    ui_->commodityCommodityCodeEdit->setText(
        QString::fromStdString(commodityInstrument_.commodity_code));
    ui_->commodityCurrencyEdit->setText(
        QString::fromStdString(commodityInstrument_.currency));
    ui_->commodityQuantitySpinBox->setValue(commodityInstrument_.quantity);
    ui_->commodityUnitEdit->setText(
        QString::fromStdString(commodityInstrument_.unit));
    ui_->commodityStartDateEdit->setText(
        QString::fromStdString(commodityInstrument_.start_date));
    ui_->commodityMaturityDateEdit->setText(
        QString::fromStdString(commodityInstrument_.maturity_date));
    ui_->commodityFixedPriceSpinBox->setValue(
        commodityInstrument_.fixed_price.value_or(0.0));
    ui_->commodityOptionTypeEdit->setText(
        QString::fromStdString(commodityInstrument_.option_type));
    ui_->commodityStrikePriceSpinBox->setValue(
        commodityInstrument_.strike_price.value_or(0.0));
    ui_->commodityExerciseTypeEdit->setText(
        QString::fromStdString(commodityInstrument_.exercise_type));
    ui_->commodityAverageTypeEdit->setText(
        QString::fromStdString(commodityInstrument_.average_type));
    ui_->commodityAveragingStartDateEdit->setText(
        QString::fromStdString(commodityInstrument_.averaging_start_date));
    ui_->commodityAveragingEndDateEdit->setText(
        QString::fromStdString(commodityInstrument_.averaging_end_date));
    ui_->commoditySpreadCommodityCodeEdit->setText(
        QString::fromStdString(commodityInstrument_.spread_commodity_code));
    ui_->commoditySpreadAmountSpinBox->setValue(
        commodityInstrument_.spread_amount.value_or(0.0));
    ui_->commodityStripFrequencyCodeEdit->setText(
        QString::fromStdString(commodityInstrument_.strip_frequency_code));
    ui_->commodityVarianceStrikeSpinBox->setValue(
        commodityInstrument_.variance_strike.value_or(0.0));
    ui_->commodityAccumulationAmountSpinBox->setValue(
        commodityInstrument_.accumulation_amount.value_or(0.0));
    ui_->commodityKnockOutBarrierSpinBox->setValue(
        commodityInstrument_.knock_out_barrier.value_or(0.0));
    ui_->commodityBarrierTypeEdit->setText(
        QString::fromStdString(commodityInstrument_.barrier_type));
    ui_->commodityLowerBarrierSpinBox->setValue(
        commodityInstrument_.lower_barrier.value_or(0.0));
    ui_->commodityUpperBarrierSpinBox->setValue(
        commodityInstrument_.upper_barrier.value_or(0.0));
    ui_->commodityBasketJsonEdit->setPlainText(
        QString::fromStdString(commodityInstrument_.basket_json));
    ui_->commodityDayCountCodeEdit->setText(
        QString::fromStdString(commodityInstrument_.day_count_code));
    ui_->commodityPaymentFrequencyCodeEdit->setText(
        QString::fromStdString(commodityInstrument_.payment_frequency_code));
    ui_->commoditySwaptionExpiryDateEdit->setText(
        QString::fromStdString(commodityInstrument_.swaption_expiry_date));
    ui_->commodityDescriptionEdit->setPlainText(
        QString::fromStdString(commodityInstrument_.description));
    block(false);

    ui_->instrumentProvenanceWidget->populate(
        commodityInstrument_.version,
        commodityInstrument_.modified_by,
        commodityInstrument_.performed_by,
        commodityInstrument_.recorded_at,
        commodityInstrument_.change_reason_code,
        commodityInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateCommodityTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateCommodityInstrumentFromUi() {
    commodityInstrument_.trade_type_code =
        ui_->commodityTradeTypeCodeEdit->text().trimmed().toStdString();
    commodityInstrument_.commodity_code =
        ui_->commodityCommodityCodeEdit->text().trimmed().toStdString();
    commodityInstrument_.currency =
        ui_->commodityCurrencyEdit->text().trimmed().toStdString();
    commodityInstrument_.quantity = ui_->commodityQuantitySpinBox->value();
    commodityInstrument_.unit =
        ui_->commodityUnitEdit->text().trimmed().toStdString();
    commodityInstrument_.start_date =
        ui_->commodityStartDateEdit->text().trimmed().toStdString();
    commodityInstrument_.maturity_date =
        ui_->commodityMaturityDateEdit->text().trimmed().toStdString();
    {
        const double v = ui_->commodityFixedPriceSpinBox->value();
        commodityInstrument_.fixed_price = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    commodityInstrument_.option_type =
        ui_->commodityOptionTypeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->commodityStrikePriceSpinBox->value();
        commodityInstrument_.strike_price = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    commodityInstrument_.exercise_type =
        ui_->commodityExerciseTypeEdit->text().trimmed().toStdString();
    commodityInstrument_.average_type =
        ui_->commodityAverageTypeEdit->text().trimmed().toStdString();
    commodityInstrument_.averaging_start_date =
        ui_->commodityAveragingStartDateEdit->text().trimmed().toStdString();
    commodityInstrument_.averaging_end_date =
        ui_->commodityAveragingEndDateEdit->text().trimmed().toStdString();
    commodityInstrument_.spread_commodity_code =
        ui_->commoditySpreadCommodityCodeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->commoditySpreadAmountSpinBox->value();
        commodityInstrument_.spread_amount = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    commodityInstrument_.strip_frequency_code =
        ui_->commodityStripFrequencyCodeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->commodityVarianceStrikeSpinBox->value();
        commodityInstrument_.variance_strike = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->commodityAccumulationAmountSpinBox->value();
        commodityInstrument_.accumulation_amount = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->commodityKnockOutBarrierSpinBox->value();
        commodityInstrument_.knock_out_barrier = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    commodityInstrument_.barrier_type =
        ui_->commodityBarrierTypeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->commodityLowerBarrierSpinBox->value();
        commodityInstrument_.lower_barrier = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->commodityUpperBarrierSpinBox->value();
        commodityInstrument_.upper_barrier = (v != 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    commodityInstrument_.basket_json =
        ui_->commodityBasketJsonEdit->toPlainText().trimmed().toStdString();
    commodityInstrument_.day_count_code =
        ui_->commodityDayCountCodeEdit->text().trimmed().toStdString();
    commodityInstrument_.payment_frequency_code =
        ui_->commodityPaymentFrequencyCodeEdit->text().trimmed().toStdString();
    commodityInstrument_.swaption_expiry_date =
        ui_->commoditySwaptionExpiryDateEdit->text().trimmed().toStdString();
    commodityInstrument_.description =
        ui_->commodityDescriptionEdit->toPlainText().trimmed().toStdString();
    commodityInstrument_.modified_by = username_;
    commodityInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateCommodityTabVisibility() {
    const QString tradeType = ui_->commodityTradeTypeCodeEdit->text().trimmed();
    const bool showCore = instrumentLoaded_ && !tradeType.isEmpty();
    const bool showExtensions = instrumentLoaded_ &&
        (isCommodityExtensionType(tradeType) ||
         !qFuzzyIsNull(ui_->commodityFixedPriceSpinBox->value()) ||
         !qFuzzyIsNull(ui_->commodityStrikePriceSpinBox->value()) ||
         !ui_->commoditySpreadCommodityCodeEdit->text().trimmed().isEmpty() ||
         !qFuzzyIsNull(ui_->commodityVarianceStrikeSpinBox->value()) ||
         !ui_->commoditySwaptionExpiryDateEdit->text().trimmed().isEmpty());

    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->commodityCoreTab), showCore);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->commodityExtensionsTab), showExtensions);
}

void TradeDetailDialog::setCommodityReadOnly(bool readOnly) {
    ui_->commodityTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->commodityCommodityCodeEdit->setReadOnly(readOnly);
    ui_->commodityCurrencyEdit->setReadOnly(readOnly);
    ui_->commodityQuantitySpinBox->setReadOnly(readOnly);
    ui_->commodityUnitEdit->setReadOnly(readOnly);
    ui_->commodityStartDateEdit->setReadOnly(readOnly);
    ui_->commodityMaturityDateEdit->setReadOnly(readOnly);
    ui_->commodityFixedPriceSpinBox->setReadOnly(readOnly);
    ui_->commodityOptionTypeEdit->setReadOnly(readOnly);
    ui_->commodityStrikePriceSpinBox->setReadOnly(readOnly);
    ui_->commodityExerciseTypeEdit->setReadOnly(readOnly);
    ui_->commodityAverageTypeEdit->setReadOnly(readOnly);
    ui_->commodityAveragingStartDateEdit->setReadOnly(readOnly);
    ui_->commodityAveragingEndDateEdit->setReadOnly(readOnly);
    ui_->commoditySpreadCommodityCodeEdit->setReadOnly(readOnly);
    ui_->commoditySpreadAmountSpinBox->setReadOnly(readOnly);
    ui_->commodityStripFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->commodityVarianceStrikeSpinBox->setReadOnly(readOnly);
    ui_->commodityAccumulationAmountSpinBox->setReadOnly(readOnly);
    ui_->commodityKnockOutBarrierSpinBox->setReadOnly(readOnly);
    ui_->commodityBarrierTypeEdit->setReadOnly(readOnly);
    ui_->commodityLowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->commodityUpperBarrierSpinBox->setReadOnly(readOnly);
    ui_->commodityBasketJsonEdit->setReadOnly(readOnly);
    ui_->commodityDayCountCodeEdit->setReadOnly(readOnly);
    ui_->commodityPaymentFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->commoditySwaptionExpiryDateEdit->setReadOnly(readOnly);
    ui_->commodityDescriptionEdit->setReadOnly(readOnly);
}

void TradeDetailDialog::saveCommodityThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::commodity_instrument& instrument) {

    struct CommoditySaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CommoditySaveResult>(self);
    connect(watcher, &QFutureWatcher<CommoditySaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Commodity instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save commodity instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Commodity instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument]() -> CommoditySaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_commodity_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

// ---------------------------------------------------------------------------
// Composite instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadCompositeInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const auto family = PT::composite;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct CompositeResult {
        bool success;
        std::string message;
        trading::domain::composite_instrument instrument;
        std::vector<trading::domain::composite_leg> legs;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CompositeResult>(self);
    connect(watcher, &QFutureWatcher<CompositeResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load composite instrument: " << result.message;
            return;
        }

        self->compositeInstrument_ = std::move(result.instrument);
        self->compositeLegs_ = std::move(result.legs);
        self->instrumentLoaded_ = true;
        self->populateCompositeInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> CompositeResult {
        if (!cm)
            return {false, "Dialog closed", {}, {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}, {}};
        if (!r->success)
            return {false, r->message, {}, {}};

        const auto* comp =
            std::get_if<trading::messaging::composite_export_result>(&r->instrument);
        if (!comp)
            return {false, "Unexpected instrument type in response", {}, {}};

        return {true, {}, comp->instrument, comp->legs};
    }));
}

void TradeDetailDialog::populateCompositeInstrument() {
    ui_->compositeTradeTypeCombo->blockSignals(true);
    ui_->compositeDescriptionEdit->blockSignals(true);
    ui_->compositeLegsWidget->blockSignals(true);

    const auto idx = ui_->compositeTradeTypeCombo->findText(
        QString::fromStdString(compositeInstrument_.trade_type_code));
    if (idx >= 0)
        ui_->compositeTradeTypeCombo->setCurrentIndex(idx);
    ui_->compositeDescriptionEdit->setPlainText(
        QString::fromStdString(compositeInstrument_.description));
    ui_->compositeLegsWidget->setLegs(compositeLegs_);

    ui_->compositeTradeTypeCombo->blockSignals(false);
    ui_->compositeDescriptionEdit->blockSignals(false);
    ui_->compositeLegsWidget->blockSignals(false);

    ui_->instrumentProvenanceWidget->populate(
        compositeInstrument_.version,
        compositeInstrument_.modified_by,
        compositeInstrument_.performed_by,
        compositeInstrument_.recorded_at,
        compositeInstrument_.change_reason_code,
        compositeInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateCompositeTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateCompositeInstrumentFromUi() {
    compositeInstrument_.trade_type_code =
        ui_->compositeTradeTypeCombo->currentText().trimmed().toStdString();
    compositeInstrument_.description =
        ui_->compositeDescriptionEdit->toPlainText().trimmed().toStdString();
    compositeLegs_ = ui_->compositeLegsWidget->legs();
    compositeInstrument_.modified_by = username_;
    compositeInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateCompositeTabVisibility() {
    const bool show = instrumentLoaded_;
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->compositeCoreTab), show);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->compositeLegsTab), show);
}

void TradeDetailDialog::setCompositeReadOnly(bool readOnly) {
    ui_->compositeTradeTypeCombo->setEnabled(!readOnly);
    ui_->compositeDescriptionEdit->setReadOnly(readOnly);
    ui_->compositeLegsWidget->setReadOnly(readOnly);
}

void TradeDetailDialog::saveCompositeThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::composite_instrument& instrument,
    const std::vector<trading::domain::composite_leg>& legs) {

    struct CompositeSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<CompositeSaveResult>(self);
    connect(watcher, &QFutureWatcher<CompositeSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Composite instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save composite instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Composite instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument, legs]() -> CompositeSaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_composite_instrument_request req;
        req.data = instrument;
        req.legs = legs;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

// ---------------------------------------------------------------------------
// Scripted instrument support
// ---------------------------------------------------------------------------

void TradeDetailDialog::loadScriptedInstrument() {
    if (!clientManager_ || !trade_.instrument_id.has_value()) return;

    const auto family = PT::scripted;
    const std::string id = boost::uuids::to_string(*trade_.instrument_id);

    struct ScriptedResult {
        bool success;
        std::string message;
        trading::domain::scripted_instrument instrument;
    };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<ScriptedResult>(self);
    connect(watcher, &QFutureWatcher<ScriptedResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load scripted instrument: " << result.message;
            emit self->errorMessage(QString::fromStdString(result.message));
            return;
        }

        self->scriptedInstrument_ = std::move(result.instrument);
        self->instrumentLoaded_ = true;
        self->populateScriptedInstrument();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, family, id]() -> ScriptedResult {
        if (!cm)
            return {false, "Dialog closed", {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = family;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}};
        if (!r->success)
            return {false, r->message, {}};

        const auto* inst =
            std::get_if<trading::domain::scripted_instrument>(&r->instrument);
        if (!inst)
            return {false, "Unexpected instrument type in response", {}};

        return {true, {}, *inst};
    }));
}

void TradeDetailDialog::populateScriptedInstrument() {
    ui_->scriptTradeTypeCodeEdit->blockSignals(true);
    ui_->scriptNameEdit->blockSignals(true);
    ui_->scriptDescriptionEdit->blockSignals(true);
    ui_->scriptBodyEdit->blockSignals(true);
    ui_->scriptEventsJsonEdit->blockSignals(true);
    ui_->scriptUnderlyingsJsonEdit->blockSignals(true);
    ui_->scriptParametersJsonEdit->blockSignals(true);

    ui_->scriptTradeTypeCodeEdit->setText(
        QString::fromStdString(scriptedInstrument_.trade_type_code));
    ui_->scriptNameEdit->setText(
        QString::fromStdString(scriptedInstrument_.script_name));
    ui_->scriptDescriptionEdit->setPlainText(
        QString::fromStdString(scriptedInstrument_.description));
    ui_->scriptBodyEdit->setPlainText(
        QString::fromStdString(scriptedInstrument_.script_body));
    ui_->scriptEventsJsonEdit->setPlainText(
        QString::fromStdString(scriptedInstrument_.events_json));
    ui_->scriptUnderlyingsJsonEdit->setPlainText(
        QString::fromStdString(scriptedInstrument_.underlyings_json));
    ui_->scriptParametersJsonEdit->setPlainText(
        QString::fromStdString(scriptedInstrument_.parameters_json));

    ui_->scriptTradeTypeCodeEdit->blockSignals(false);
    ui_->scriptNameEdit->blockSignals(false);
    ui_->scriptDescriptionEdit->blockSignals(false);
    ui_->scriptBodyEdit->blockSignals(false);
    ui_->scriptEventsJsonEdit->blockSignals(false);
    ui_->scriptUnderlyingsJsonEdit->blockSignals(false);
    ui_->scriptParametersJsonEdit->blockSignals(false);

    ui_->instrumentProvenanceWidget->populate(
        scriptedInstrument_.version,
        scriptedInstrument_.modified_by,
        scriptedInstrument_.performed_by,
        scriptedInstrument_.recorded_at,
        scriptedInstrument_.change_reason_code,
        scriptedInstrument_.change_commentary);
    ui_->instrumentProvenanceGroup->setVisible(true);

    instrumentHasChanges_ = false;
    updateScriptedTabVisibility();
    updateSaveButtonState();
}

void TradeDetailDialog::updateScriptedInstrumentFromUi() {
    scriptedInstrument_.trade_type_code =
        ui_->scriptTradeTypeCodeEdit->text().trimmed().toStdString();
    scriptedInstrument_.script_name =
        ui_->scriptNameEdit->text().trimmed().toStdString();
    scriptedInstrument_.description =
        ui_->scriptDescriptionEdit->toPlainText().trimmed().toStdString();
    scriptedInstrument_.script_body =
        ui_->scriptBodyEdit->toPlainText().toStdString();
    scriptedInstrument_.events_json =
        ui_->scriptEventsJsonEdit->toPlainText().toStdString();
    scriptedInstrument_.underlyings_json =
        ui_->scriptUnderlyingsJsonEdit->toPlainText().toStdString();
    scriptedInstrument_.parameters_json =
        ui_->scriptParametersJsonEdit->toPlainText().toStdString();
    scriptedInstrument_.modified_by = username_;
    scriptedInstrument_.performed_by = username_;
}

void TradeDetailDialog::updateScriptedTabVisibility() {
    const bool show = instrumentLoaded_;
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->scriptDefinitionTab), show);
    ui_->tabWidget->setTabVisible(
        ui_->tabWidget->indexOf(ui_->scriptBodyTab), show);
}

void TradeDetailDialog::setScriptedReadOnly(bool readOnly) {
    ui_->scriptTradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->scriptNameEdit->setReadOnly(readOnly);
    ui_->scriptDescriptionEdit->setReadOnly(readOnly);
    ui_->scriptBodyEdit->setReadOnly(readOnly);
    ui_->scriptEventsJsonEdit->setReadOnly(readOnly);
    ui_->scriptUnderlyingsJsonEdit->setReadOnly(readOnly);
    ui_->scriptParametersJsonEdit->setReadOnly(readOnly);
}

void TradeDetailDialog::saveScriptedThenTrade(
    const trading::domain::trade& trade,
    const trading::domain::scripted_instrument& instrument) {

    struct ScriptedSaveResult { bool success; std::string message; };

    QPointer<TradeDetailDialog> self = this;
    auto* watcher = new QFutureWatcher<ScriptedSaveResult>(self);
    connect(watcher, &QFutureWatcher<ScriptedSaveResult>::finished,
            self, [self, watcher, trade]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Scripted instrument save failed: "
                                       << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed",
                tr("Failed to save scripted instrument:\n%1").arg(errorMsg));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Scripted instrument saved; saving trade";
        self->instrumentHasChanges_ = false;
        self->saveTrade(trade);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument]() -> ScriptedSaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_scripted_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
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
