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
#include "ores.qt/FxPairDialog.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPointer>
#include <QSignalBlocker>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cctype>
#include <utility>

namespace ores::qt {

using namespace ores::logging;

namespace {

std::string to_lower(const std::string& s) {
    std::string r = s;
    std::transform(r.begin(), r.end(), r.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return r;
}

std::string derive_ore_key(const std::string& base, const std::string& quote) {
    if (base.empty() || quote.empty())
        return {};
    return "FX/RATE/" + base + "/" + quote;
}

std::string derive_source_name(const std::string& base, const std::string& quote) {
    if (base.empty() || quote.empty())
        return {};
    return "synthetic." + to_lower(base) + to_lower(quote);
}

}

FxPairDialog::FxPairDialog(ClientManager* cm,
                           const QString& username,
                           const boost::uuids::uuid& parentFeedId,
                           QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , imageCache_(new ImageCache(cm, this))
    , isNew_(true) {

    fx_.id = boost::uuids::random_generator()();
    fx_.config_id = parentFeedId;
    fx_.party_id = clientManager_->currentPartyId();
    fx_.gmm_initial_price = 1.0;
    fx_.ticks_per_hour = 60;
    fx_.enabled = true;

    BOOST_LOG_SEV(lg(), info) << "Opening fx pair dialog for a new pair "
                              << boost::uuids::to_string(fx_.id) << " under feed "
                              << boost::uuids::to_string(parentFeedId) << ".";
    buildUi();
}

FxPairDialog::FxPairDialog(ClientManager* cm,
                           const QString& username,
                           const synthetic::domain::fx_spot_generation_config& existing,
                           QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , imageCache_(new ImageCache(cm, this))
    , isNew_(false)
    , fx_(existing) {

    BOOST_LOG_SEV(lg(), info) << "Opening fx pair dialog editing pair "
                              << boost::uuids::to_string(fx_.id) << ".";
    buildUi();
}

void FxPairDialog::buildUi() {
    setWindowTitle(isNew_ ? tr("New FX Pair") : tr("Edit FX Pair"));
    setModal(true);

    auto* layout = new QVBoxLayout(this);
    auto* form = new QFormLayout();

    baseCombo_ = new QComboBox(this);
    quoteCombo_ = new QComboBox(this);
    oreKeyLabel_ = new QLabel(this);
    oreKeyLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    sourceNameLabel_ = new QLabel(this);
    sourceNameLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);

    priceSpin_ = new QDoubleSpinBox(this);
    priceSpin_->setRange(0.0001, 1e9);
    priceSpin_->setDecimals(4);
    priceSpin_->setValue(fx_.gmm_initial_price > 0 ? fx_.gmm_initial_price : 1.0);

    ticksSpin_ = new QSpinBox(this);
    ticksSpin_->setRange(1, 100000);
    ticksSpin_->setValue(fx_.ticks_per_hour > 0 ? fx_.ticks_per_hour : 60);

    enabledCheck_ = new QCheckBox(tr("Enabled"), this);
    enabledCheck_->setChecked(fx_.enabled);

    form->addRow(tr("Base currency"), baseCombo_);
    form->addRow(tr("Quote currency"), quoteCombo_);
    form->addRow(tr("ORE key"), oreKeyLabel_);
    form->addRow(tr("Source name"), sourceNameLabel_);
    form->addRow(tr("Initial price"), priceSpin_);
    form->addRow(tr("Ticks / hr"), ticksSpin_);
    form->addRow(QString(), enabledCheck_);
    layout->addLayout(form);

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttons);
    connect(buttons, &QDialogButtonBox::accepted, this, &FxPairDialog::onSave);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);

    connect(baseCombo_, &QComboBox::currentTextChanged, this,
            &FxPairDialog::onCurrencyChanged);
    connect(quoteCombo_, &QComboBox::currentTextChanged, this,
            &FxPairDialog::onCurrencyChanged);

    imageCache_->loadAll();
    populateCurrencyCombo(baseCombo_);
    populateCurrencyCombo(quoteCombo_);

    recomputeDerived();
}

void FxPairDialog::populateCurrencyCombo(QComboBox* combo) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<FxPairDialog> self = this;
    QPointer<QComboBox> target = combo;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<std::string> {
        return fetch_currency_codes(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self,
            [self, target, watcher]() {
                auto codes = watcher->result();
                watcher->deleteLater();
                if (!self || !target)
                    return;

                // Preselect the value carried by the entity being edited.
                const QString preselect = (target == self->baseCombo_)
                    ? QString::fromStdString(self->fx_.base_currency_code)
                    : QString::fromStdString(self->fx_.quote_currency_code);

                const QSignalBlocker blocker(target);
                target->clear();
                target->addItem(QString()); // "(select)" sentinel
                for (const auto& code : codes) {
                    target->addItem(QString::fromStdString(code));
                }

                apply_flag_icons(target, self->imageCache_, FlagSource::Currency);

                if (!preselect.isEmpty())
                    target->setCurrentText(preselect);

                self->recomputeDerived();
            });

    watcher->setFuture(QtConcurrent::run(task));
}

void FxPairDialog::recomputeDerived() {
    const auto base = baseCombo_->currentText().toStdString();
    const auto quote = quoteCombo_->currentText().toStdString();
    oreKeyLabel_->setText(QString::fromStdString(derive_ore_key(base, quote)));
    sourceNameLabel_->setText(QString::fromStdString(derive_source_name(base, quote)));
}

void FxPairDialog::onCurrencyChanged() {
    recomputeDerived();
}

void FxPairDialog::onSave() {
    const auto base = baseCombo_->currentText().toStdString();
    const auto quote = quoteCombo_->currentText().toStdString();

    if (base.empty() || quote.empty()) {
        QMessageBox::warning(this, tr("Incomplete"),
                             tr("Both base and quote currencies must be set."));
        return;
    }
    if (base == quote) {
        QMessageBox::warning(this, tr("Invalid pair"),
                             tr("Base and quote currencies must differ."));
        return;
    }

    auto fx = fx_;
    fx.base_currency_code = base;
    fx.quote_currency_code = quote;
    fx.ore_key = derive_ore_key(base, quote);
    fx.source_name = derive_source_name(base, quote);
    fx.gmm_initial_price = priceSpin_->value();
    fx.ticks_per_hour = ticksSpin_->value();
    fx.enabled = enabledCheck_->isChecked();
    fx.modified_by = username_.toStdString();
    fx.change_reason_code = isNew_ ? "system.new_record" : "common.non_material_update";
    fx.change_commentary = "Authored via Market Simulator";
    fx.version = 0;

    const std::string id = boost::uuids::to_string(fx.id);
    BOOST_LOG_SEV(lg(), info) << "Saving fx pair " << id << " (" << base << "/" << quote
                              << ", new=" << isNew_ << ").";

    QPointer<FxPairDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm, fx]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(
            synthetic::messaging::save_fx_spot_generation_config_request::from(fx));
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, id]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Save failed for fx pair " << id << ": " << err.toStdString();
                    QMessageBox::critical(self, self->tr("Save failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Saved fx pair " << id << ".";
                self->accept();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

}
