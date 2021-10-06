syms x_0 x_1

f = sqrt(x_0 ^ 2 + x_1 ^ 2) - 2;
objective = f^2;

grad_f = gradient(f);

x_axis = linspace(-10, 10, 100);
y_axis = linspace(-10, 10, 100);
z = zeros(length(y_axis), length(x_axis));

h = matlabFunction(objective);
for xx = (1:length(x_axis))
  for yy = (1:length(y_axis))
    z(yy, xx) = h(x_axis(xx), y_axis(yy));
  end
end

imagesc(x_axis, y_axis, log10(z));