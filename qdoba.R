# Here we simulate a day in the fast food industry (Qdoba, in this case). This
library(doParallel)
registerDoParallel(2)

cat0 <- function(...) {
  cat(..., sep = "")
} # Concatenate without automatic separation

# Server states:
IDLE <- 0
BUSY <- 1

# Main simulation:
run_qdoba_sim <- function(arrival.rate = 1.25, service.rate = 4, num.mins.open = 1000, live = T) {
  if(live) cat("Good morning! ")
  stock <- floor(runif(10, 15, 31))

  next.arrival <- rexp(1, arrival.rate)
  next.service <- Inf

  total.customers.served <- 0
  max.line.size <- 0
  time.spent.busy <- 0
  total.cumulative.line.size <- 0

  arrival.times <- vector()
  service.times <- vector()

  server.state <- IDLE
  num.in.line <- 0
  sim.clock <- 0

  while(sim.clock <= num.mins.open  ||  server.state == BUSY) {
    if(sim.clock > 0)  service.rate <- max(arrival.rate/2, service.rate - max(0, time.spent.busy/num.mins.open - 0.75)/1000)
    # Compute the time of the next event.
    time.of.next.event <- min(next.arrival, next.service)

    # How much time has elapsed between the previous event and this
    # one?
    time.since.last.event <- time.of.next.event - sim.clock

    # Compute the height of the rectangles we need to add to our
    # running totals. (This is the state that the server was just in,
    # and the number of people in line there just were.)
    height.of.util.rectangle <- server.state
    height.of.num.in.line.rectangle <- num.in.line

    # Compute the areas of the rectangles we need to add to our
    # running totals.
    area.of.util.rectangle <- time.since.last.event*height.of.util.rectangle
    area.of.num.in.line.rectangle <- time.since.last.event*height.of.num.in.line.rectangle

    # Add these areas to our running totals.
    time.spent.busy <- time.spent.busy + area.of.util.rectangle
    total.cumulative.line.size <- total.cumulative.line.size + area.of.num.in.line.rectangle

    # Okay, now that that bookkeeping's out of the way, actually
    # advance the simulation clock.
    sim.clock <- time.of.next.event

    if(next.arrival < next.service) {
      if(live) cat0("A customer arrived at ", sim.clock, ".\n")
      arrival.times <- c(arrival.times, next.arrival)
      # The next event to happen is an arrival.
      if(server.state == BUSY) {
        num.in.line <- num.in.line + 1
        if(num.in.line > max.line.size) {
          max.line.size <- num.in.line
        }
        if(live) cat0(" Had to get in line. Line is now ", num.in.line, ifelse(num.in.line == 1, " person", " people"), " long.\n")
      } else {
        if(live) cat0(" Step right up!\n")
        server.state <- BUSY
        next.service <- sim.clock + rexp(1, service.rate)
      }
      if(sim.clock <= num.mins.open) {
        if(num.in.line < round(runif(1, 10, 20))) {
          next.arrival <- sim.clock + rexp(1, arrival.rate)
        } else {
          next.arrival <- sim.clock + rexp(1, arrival.rate + 1)
        }
      } else {
        if(live) cat0("Sorry, bub, we're closed!!\n")
        next.arrival <- Inf
      }
    } else {
      if(live) cat0("A customer got served at ", sim.clock, ".\n")
      condiments <- sample(10, floor(runif(1, 1, 5)))
      stock[condiments] <- stock[condiments] - 1
      service.times <- c(service.times, next.service)
      total.customers.served <- total.customers.served + 1
      # The next event to happen is a service.
      if(num.in.line > 0) {
        if(live) cat0(" The line advances! It now has ", num.in.line, ifelse(num.in.line == 1, " person", " people"), " in it.\n")
        num.in.line <- num.in.line - 1
        next.service <- sim.clock + rexp(1, service.rate) + ifelse(0%in%stock, 5, 0)
      } else {
        if(live) cat0("Whew! Get to sit down finally!\n")
        server.state <- IDLE
        next.service <- Inf
      }
      if(0%in%stock) {
        none <- grep(0, stock)
        if(live) cat0("Restocking ", length(none), ifelse(length(none) > 1, " items!\n", " item!\n"))
        stock[none] <- 20
      }
    }
  }

  if(live) cat0("It's now quittin' time!\n")
  cat0("We served ", total.customers.served, " customers today!\n")
  cat0("The longest the line ever got was ", max.line.size, ".\n")
  cat0("The server was on his feet ", round(time.spent.busy/sim.clock, 2)*100, "% of the time.\n")
  cat0("The average line size was ", round(total.cumulative.line.size/sim.clock, 2), " people.\n")

  delays <- service.times - arrival.times

  cat0("The average delay in line was ", floor(mean(delays)/60), ifelse(floor(mean(delays)/60) == 1," hour and "," hours and "), round(mean(delays)%%60, 2), " minutes long.\n")

  cat0("The store was open for ", floor(sim.clock/60), " hours and ", round(sim.clock%%60, 2), " minutes.\n")

  return(data.frame(arrival.rate = arrival.rate,
                    service.rate = service.rate,
                    total.customers.served = total.customers.served,
                    max.line.size = max.line.size,
                    utilization = time.spent.busy/sim.clock*100,
                    average.line.size = total.cumulative.line.size/sim.clock,
                    average.delay = mean(delays),
                    time.store.close = sim.clock))
}

results <- foreach(service.rate = seq(0.05, 3, 0.05), .combine = rbind) %dopar% {
  return(run_qdoba_sim(arrival.rate = 1, service.rate = service.rate))
}

for(col in 3:ncol(results)) {
  plot(results$service.rate, results[[col]], type = "l", col = "deepskyblue4", main = names(results)[col])
}
# The end
